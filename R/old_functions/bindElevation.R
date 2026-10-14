bindElevation <- function(d) {
    library(elevatr)

    # Check if required columns exist
    if (!all(c("latitude", "longitude") %in% colnames(d))) {
        stop("The dataframe must contain 'latitude' and 'longitude' columns.")
    }

    # Ensure latitude and longitude are numeric
    d$latitude <- as.numeric(d$latitude)
    d$longitude <- as.numeric(d$longitude)

    # Remove rows with missing coordinates
    d <- d[!is.na(d$latitude) & !is.na(d$longitude), ]

    # Rename columns to match expected input for get_elev_point
    points <- data.frame(x = d$longitude, y = d$latitude)

    # Get elevation data
    elevation_data <- get_elev_point(points, prj = "+proj=longlat +datum=WGS84", src = "aws")

    # Add elevation to the original dataframe
    d$elevation <- elevation_data$elevation

    return(d)
}
