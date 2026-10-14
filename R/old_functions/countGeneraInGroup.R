
countGeneraInGroup <- function(higher_group) {

    library(rgbif)
    # Step 1: Get the taxon key for the family
    higher_group_key <- name_suggest(q = higher_group, rank = "family")$key[1]

    if (is.null(higher_group_key)) {
        stop("The higher group name could not be resolved. Please check your input.")
    }

    # Step 2: Retrieve all genera under the family
    offset <- 0
    limit <- 1000  # Maximum limit per request
    all_genera <- character()  # Initialize empty vector for genera

    repeat {
        response <- name_lookup(
            higherTaxonKey = higher_group_key,
            rank = "genus",
            status = "ACCEPTED",
            limit = limit,
            start = offset
        )

        if (is.null(response$data) || nrow(response$data) == 0) {
            break  # Stop if no more data is available
        }

        # Extract genus names
        genera_batch <- response$data$canonicalName
        all_genera <- c(all_genera, genera_batch)

        # Update the offset for the next batch
        offset <- offset + limit

        if (nrow(response$data) < limit) {
            break  # Stop if fewer results than the limit
        }
    }

    # Remove duplicates (if any)
    all_genera <- unique(all_genera)

    cat("Number of genera retrieved:", length(all_genera), "\n")

    return(all_genera)

    # Step 3: Retrieve all species for each genus
    all_species <- character()  # Initialize empty vector for species

    for (genus in all_genera) {
        cat("Fetching species for genus:", genus, "\n")
        genus_key <- name_suggest(q = genus, rank = "genus")$key[1]

        if (is.null(genus_key)) {
            next  # Skip if the genus key cannot be resolved
        }

        genus_offset <- 0

        repeat {
            response <- name_lookup(
                higherTaxonKey = genus_key,
                rank = "species",
                status = "ACCEPTED",
                limit = limit,
                start = genus_offset
            )

            if (is.null(response$data) || nrow(response$data) == 0) {
                break  # Stop if no more data is available
            }

            # Extract species names
            species_batch <- response$data$canonicalName
            all_species <- c(all_species, species_batch)

            # Update the offset for the next batch
            genus_offset <- genus_offset + limit

            if (nrow(response$data) < limit) {
                break  # Stop if fewer results than the limit
            }
        }
    }

    # Remove duplicates (if any)
    all_species <- unique(all_species)
    cat("Number of species retrieved:", length(all_species), "\n")

    #return()
}

# Example Usage
#result <- get_all_species_by_family("Carabidae")
