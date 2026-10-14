# ============================================================
# Summarize the contribution of Observation.org data relative
# to iNaturalist data in a combined GBIF occurrence download.
#
# Goal: for each species, count records from iNaturalist alone
# vs. records from iNaturalist + Observation.org combined, and
# find species that cross the 10-obs and 20-obs thresholds only
# once obs.org data is added.
# ============================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)

# ------------------------------------------------------------
# 1. Read the data
# ------------------------------------------------------------
# GBIF occurrence.txt files are tab-delimited and can contain
# stray quote characters (e.g. inside dynamicProperties JSON),
# so we disable quoting entirely to avoid parsing errors.
#
# For an ~8GB file, only pull in the columns this analysis
# actually needs. col_select avoids allocating memory for the
# ~190 unused GBIF columns, and explicit col_types skips the
# (slow, memory-heavy) type-guessing pass entirely.
needed_cols <- c(
    "gbifID",
    "species",
    "taxonKey",
    "institutionCode"
)

occ <- read_tsv(
    "D:/0036785-260806074905277/occurrence.txt",
    quote = "",
    col_select = all_of(needed_cols),
    col_types = cols(
        gbifID          = col_character(),
        species         = col_character(),
        taxonKey        = col_character(),
        institutionCode = col_character(),
        .default        = col_skip()
    ),
    na = c("", "NA"),
    show_col_types = FALSE
)

# ------------------------------------------------------------
# 1b. (Alternative, often faster on very large files)
# ------------------------------------------------------------
# data.table::fread scans the file once and only materializes
# the requested columns, which tends to be noticeably faster
# than readr on multi-GB files. Uncomment to use instead:
#
# library(data.table)
# occ <- fread(
#   "D:/0036785-260806074905277/occurrence.txt",
#   sep = "\t",
#   quote = "",
#   select = needed_cols,
#   na.strings = c("", "NA"),
#   encoding = "UTF-8"
# )
# setDF(occ)  # convert data.table -> data.frame for the dplyr code below

# ------------------------------------------------------------
# 2. Classify each record's data source
# ------------------------------------------------------------
# Binary split: institutionCode == "iNaturalist" is iNaturalist;
# everything else in this download is treated as Observation.org.
# Use %in% rather than == so that NA institutionCode values (as
# in the obs.org rows) correctly fall through to "Observation.org"
# instead of propagating as NA.
occ <- occ %>%
    mutate(
        source = if_else(institutionCode %in% "iNaturalist", "iNaturalist", "Observation.org")
    )

# Check the split
print(count(occ, source))

# ------------------------------------------------------------
# 3. Per-species counts by source
# ------------------------------------------------------------
# Use `species` (binomial) as the grouping unit. Swap in
# `scientificName` or `taxonKey` if you'd rather group at a
# different rank or avoid string-matching issues.
species_counts <- occ %>%
    count(species, source) %>%
    pivot_wider(
        names_from = source,
        values_from = n,
        values_fill = 0
    ) %>%
    rename(
        inat_n   = iNaturalist,
        obsorg_n = `Observation.org`
    ) %>%
    mutate(
        combined_n = inat_n + obsorg_n
    )

# ------------------------------------------------------------
# 4. Flag threshold crossings
# ------------------------------------------------------------
thresholds <- c(5, 10, 20, 25)

for (t in thresholds) {
    inat_col     <- paste0("inat_meets_", t)
    combined_col <- paste0("combined_meets_", t)
    newly_col    <- paste0("newly_crosses_", t)

    species_counts[[inat_col]]     <- species_counts$inat_n     >= t
    species_counts[[combined_col]] <- species_counts$combined_n >= t
    species_counts[[newly_col]]    <- species_counts[[combined_col]] & !species_counts[[inat_col]]
}

# ------------------------------------------------------------
# 5. Summarize results
# ------------------------------------------------------------
summary_table <- tibble(
    threshold = thresholds,
    species_meeting_with_inat_only = sapply(thresholds, function(t) {
        sum(species_counts[[paste0("inat_meets_", t)]])
    }),
    species_meeting_combined = sapply(thresholds, function(t) {
        sum(species_counts[[paste0("combined_meets_", t)]])
    }),
    newly_crossing_thanks_to_obsorg = sapply(thresholds, function(t) {
        sum(species_counts[[paste0("newly_crosses_", t)]])
    })
)

print(summary_table)

# List of species that newly cross each threshold, with their counts.
# Stored in a named list (one element per threshold) so this works
# automatically for however many thresholds are in `thresholds`.
newly_cross_lists <- setNames(
    lapply(thresholds, function(t) {
        species_counts %>%
            filter(.data[[paste0("newly_crosses_", t)]]) %>%
            arrange(desc(combined_n)) %>%
            select(species, inat_n, obsorg_n, combined_n)
    }),
    paste0("newly_cross_", thresholds)
)

# Print each one
for (nm in names(newly_cross_lists)) {
    cat("\n---", nm, "---\n")
    print(newly_cross_lists[[nm]])
}

# Species with zero iNaturalist records at all (100% obs.org contribution)
obsorg_only_species <- species_counts %>%
    filter(inat_n == 0, obsorg_n > 0) %>%
    arrange(desc(obsorg_n))

print(obsorg_only_species)

# ------------------------------------------------------------
# 6. (Optional) write results out
# ------------------------------------------------------------
write_csv(species_counts, "species_counts_by_source.csv")
write_csv(summary_table, "threshold_summary.csv")

for (nm in names(newly_cross_lists)) {
    write_csv(newly_cross_lists[[nm]], paste0(nm, ".csv"))
}

setdiff(newly_cross_lists$newly_cross_10$species, newly_cross_lists$newly_cross_20$species)
setdiff(newly_cross_lists$newly_cross_20$species, newly_cross_lists$newly_cross_10$species)
sum(obsorg_only_species$species %in% newly_cross_lists$newly_cross_10$species)
