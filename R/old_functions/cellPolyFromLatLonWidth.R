# Function to create a square buffer around a point (lat, lon)
cellPolyFromLatLonWidth <- function(lat, lon, dist_km) {
    library(sf)
    library(geosphere)

    dist_km <- dist_km * 0.5
    # Earth's radius in meters
    r <- 6378137
    # Convert distance from km to meters
    dist_m <- dist_km * 1000

    # Calculate offset in degrees
    lat_offset <- (dist_m / r) * (180 / pi)
    lon_offset <- (dist_m / (r * cos(pi * lat / 180))) * (180 / pi)

    # Create square points
    points_matrix <- matrix(c(lon - lon_offset, lat + lat_offset,  # Top left
                              lon + lon_offset, lat + lat_offset,  # Top right
                              lon + lon_offset, lat - lat_offset,  # Bottom right
                              lon - lon_offset, lat - lat_offset,  # Bottom left
                              lon - lon_offset, lat + lat_offset), # Closing point to make a polygon
                            ncol = 2, byrow = TRUE)

    # Create an sf polygon
    poly_sf <- st_polygon(list(points_matrix)) %>%
        st_sfc(crs = 4326) %>%
        st_as_sf()

    return(poly_sf)
}

#df$latitude <- df$cell_lat
#df$longitude <- df$cell_lon
#colorEvoHelpers::plotLatLonHeatmap(df)
