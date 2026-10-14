
bindHigherTaxon <- function(d, taxon="subfamily",save_location="data/genus_higher_taxa.csv") {
    library(taxize)

    # if higher taxa already exist in the data folder of this project, just load them rather than redoing
    if(file.exists(save_location)){
        genera_and_higher_taxa <- read.csv(save_location)
        d <- merge(d,genera_and_higher_taxa,by="genus")

        return(d)
    }

    genera <- unique(d$genus)
    classifications <- classification(genera, db = "itis", ask = FALSE) # ask = FALSE avoids manual input
    subfamilies <- sapply(classifications, function(taxon) {
        if (is.character(taxon) && grepl("no direct match", taxon)) {
            return(NA) # Handle cases where no direct match is found
        }
        if (is.character(taxon) && grepl("More than one TSN found", taxon)) {
            return(NA) # Handle cases with multiple TSNs
        }
        if (is.data.frame(taxon)) {
            # Filter for the subfamily level
            subfamily <- taxon[taxon$rank == "subfamily", "name"]
            if (length(subfamily) > 0) {
                return(subfamily)
            } else {
                return(NA) # If no subfamily is found
            }
        } else {
            return(NA) # Handle other unexpected cases
        }
    })

    genera_and_higher_taxa <- data.frame(cbind(genera,subfamilies))
    colnames(genera_and_higher_taxa) <- c("genus","subfamily")

    write.csv(genera_and_higher_taxa,"data/genus_higher_taxa.csv")

    d <- merge(d,genera_and_higher_taxa,by="genus")

    return(d)
}
