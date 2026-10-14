
filterWellSampledSpecies <- function(d, n, plot = TRUE) {
    library(dplyr)
    library(ggplot2)

    # If plot is TRUE, create a plot of species count vs threshold
    if (plot) {
        species_counts <- d %>%
            group_by(species) %>%
            summarise(count = n(), .groups = 'drop')

        thresholds <- unique(species_counts$count)
        thresholds <- thresholds[order(thresholds)]

        threshold_data <- sapply(thresholds, function(thresh) {
            sum(species_counts$count >= thresh)
        })

        plot_data <- data.frame(
            Threshold = thresholds,
            SpeciesCount = threshold_data
        )

        ggplot(plot_data, aes(x = Threshold, y = SpeciesCount)) +
            geom_line() +
            geom_point() +
            theme_minimal() +
            labs(
                title = "Number of Species Passing Different Thresholds",
                x = "Threshold (Minimum Number of Samples)",
                y = "Number of Species"
            )
    }

    # Filter for species with at least 'n' samples
    # Alternative filtering for species with at least 'n' samples
    d2 <- d %>% group_by(species) %>% summarize(n = n()) %>% filter(n >= 5)
    species_to_keep <- unique(d2$species)

    d <- d %>%
        filter(species %in% species_to_keep)

    return(d)
}
