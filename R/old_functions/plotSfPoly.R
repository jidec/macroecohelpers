plotSfPoly <- function(poly_sf){
    library(ggspatial)
    library(ggplot2)
    library(prettymapr)
    ggplot() +
        annotation_map_tile() + # This adds a basemap
        geom_sf(data = poly_sf, fill = "transparent", color = "red", size = 2) +
        ggtitle("SF Polygon on Basemap") +
        theme_minimal()
}
