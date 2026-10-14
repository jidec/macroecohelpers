getFirstiNatImageLinks <- function(inat_obs_ids,take_urls=TRUE) {
    library(httr)
    library(jsonlite)

    if(take_urls){
        inat_obs_ids <- str_extract(inat_obs_ids, "\\d+$")
    }

    observation_ids <-  inat_obs_ids
    base_url <- "https://api.inaturalist.org/v1/observations/"
    image_urls <- vector("list", length(observation_ids)) # Initialize empty list for URLs

    # Loop through each observation ID
    for (i in seq_along(observation_ids)) {
        observation_id <- observation_ids[i]
        response <- GET(paste0(base_url, observation_id))

        if (status_code(response) == 200) {
            data <- fromJSON(content(response, "text", encoding = "UTF-8"))
            image_urls[[i]] <- data$results$photos[[1]]$url[1]
        } else {
            warning(paste("Failed to retrieve observation:", observation_id))
            image_urls[[i]] <- NA
        }
    }
    image_urls <- unlist(image_urls)
    image_urls <- str_replace(image_urls,"square","medium")
    return(unlist(image_urls))
}
