

dlFamiliesFromGenera <- function(genera,save_path){
    library(taxize)

    # vector of unique genera
    genera <- unique(genera)

    # Function to get family for a single genus with error handling
    get_family <- function(genus) {
        # Use tryCatch to handle errors
        tryCatch({
            # Get taxonomic classification
            classification_info <- classification(genus, db = "ncbi")

            # Extract family information
            family <- classification_info[[1]] %>%
                filter(rank == "family") %>%
                pull(name)

            # Return family or NA if not found
            if (length(family) > 0) {
                return(family)
            } else {
                return(NA)
            }
        }, error = function(e) {
            # Return NA if an error occurs
            return(NA)
        })
    }

    # Apply the function to each genus
    families <- sapply(genera, get_family, USE.NAMES = TRUE)

    # Combine genera and families into a data frame
    family_info <- data.frame(
        genus = genera,
        family = families
    )


    write.csv(families,save_path,row.names = FALSE)
}
