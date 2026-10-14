
# df must have:
# 1. cell_lat and cell_lon columns
# 2. species column
# 3. season column where season is "1","2","3","4"
plotScatterpie <- function(df){
    library(sf)
    library(ggplot2)
    library(rnaturalearth)
    library(rnaturalearthdata)
    library(ggforce)
    library(scatterpie)
    library(dplyr)

    #sscs_scatterpie <- readRDS("_targets/objects/sscs_final")
    #cell_data <- readRDS("_targets/objects/obs_data_gridded") %>% select(cell,cell_lat,cell_lon) %>% distinct()
    #sscs_coords <- merge(sscs_scatterpie,cell_data,by="cell")

    # summarize to each cell_lat and cell_lon
    cells <- df %>%
        group_by(cell_lat, cell_lon) %>%
        summarise(n=n(),
                  nspecies = length(unique(species)),
                  Winter = sum(season == "1"),
                  Spring = sum(season == "2"),
                  Summer = sum(season == "3"),
                  Fall = sum(season == "4"))


    highest_nspecies = max(cells$nspecies)
    print(highest_nspecies)
    highest_n = max(cells$n)
    print(highest_n)


    # Assuming 'cell_lat' and 'cell_lon' are the columns with latitude and longitude information
    # Convert the data frame to an sf object with WGS 84 (EPSG:4326) CRS
    cells_sf <- st_as_sf(cells, coords = c("cell_lon", "cell_lat"), crs = 4326, agr = "constant")

    # If you don't have the 'agr' argument, it's okay to exclude it, it's just to specify the geometry type

    # Get North America map
    north_america <- ne_countries(scale = "medium", returnclass = "sf", continent = "north america")

    lon_min <- min(cells$cell_lon)
    lon_max <- max(cells$cell_lon)
    lat_min <- min(cells$cell_lat)
    lat_max <- max(cells$cell_lat)


    world <- map_data('world')
    p <- ggplot(world, aes(long, lat)) +
        geom_map(map=world, aes(map_id=region), fill=NA, color="black") +
        coord_quickmap()
    p <- p + geom_scatterpie(aes(x=cell_lon, y=cell_lat,r=log(nspecies)/4.5), data=cells, #nspecies/60
                        cols=c("Winter","Spring","Summer","Fall")) + coord_equal() +
        geom_scatterpie_legend(log(cells$nspecies)/4.5, x=-70, y=30,n=3) +
        xlim(-130,lon_max) + ylim(lat_min,50)
    plot(p)
}
