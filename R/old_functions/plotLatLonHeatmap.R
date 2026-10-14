#' Plot a heatmap of where lat long points occur over a map of the US
#'
#' @description Used for plotting extent and bias of spatially distrubted observations
#' @param df data.frame to get "latitude" and "longitude" columns from
#' @return nothing
#'
plotLatLonHeatmap <- function(df){
    library(maps)
    library(mapdata)
    library(ggplot2)

    # Get US map data
    us_map <- map_data("state")

    # Create the plot
    ggplot(df, aes(x = longitude, y = latitude)) +
        stat_density2d(aes(fill = ..density..), geom = "tile", contour = FALSE) +
        scale_fill_gradient(low = "white", high = "red") +
        geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = NA, color = "grey") +
        coord_map("albers", lat0 = 39, lat1 = 45)
}
