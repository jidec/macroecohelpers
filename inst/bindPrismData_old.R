
# function should take the gridded sscs Df (with lat lon and size of cells)
# for each unique grid cell, create a shape file representing that grid cell
# then for each unique season cell OR season year cell:
#   get the

#df = readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/sscs2_initial")
#cell_width_km = 250
#avg_of_years = 2018:2020
#avg_of_years=NULL
#variable="tmean"
#prism::prism_set_dl_dir("D:/GitProjects/inat-daily-activity-analysis/data/prism")

# df must be gridded (with cell, cell_lat, and cell_lon columns)
# must have a season column where seasons are "1", "2", "3", "4"
# and year column
# currently based on 250 km cell width

bindPrismData <- function(df,prism_dir,cell_width_km=250,variable="tmean",new_var_colname="prism_tmean",
                          avg_of_years=NULL){
    library(dplyr)
    library(geosphere)
    library(raster)
    library(sf)
    library(rgdal)
    library(prism)
    library(colorEvoHelpers)

    # set prism dir
    prism_set_dl_dir(prism_dir)

    # keep only inside contiguous USA for Prism
    # Assuming your data frame is named df
    df <- df %>%
        filter(cell_lat >= 24.52, cell_lat <= 49.38,  # Latitude of the contiguous USA
               cell_lon >= -125.0, cell_lon <= -66.94,
               year >= 2015, year < 2024)  # Longitude of the contiguous USA

    # keep only cols we need
    df2 <- df %>% dplyr::select(year,season,cell,cell_lat,cell_lon)

    # get uniq cell, season, year combinations
    uniq <- df2 %>% distinct(cell,season,year,.keep_all = TRUE) %>% filter(!is.na(cell_lat) & !is.na(cell_lon))

    # bind polys
    uniq <- uniq %>% rowwise() %>%
        mutate(cell_poly = list(cellPolyFromLatLonWidth(cell_lat, cell_lon, cell_width_km)))

    if(!is.null(avg_of_years)){
        df <- df %>% dplyr::select(season,cell,cell_lat,cell_lon)
        uniq <- df %>% distinct(cell,season,.keep_all = TRUE) %>% filter(!is.na(cell_lat) & !is.na(cell_lon))
    }

    getYearSeasonPrismData <- function(year,season,cell_poly,variable){
        cell_poly=poly
        mons <- switch(season,
                       "1" = c(12,1,2),
                       "2" = c(3,4,5),
                       "3" = c(6,7,8),
                       "4" = c(9,10,11))
        pd <- prism_archive_subset(variable, "monthly", mon = mons, years=year)
        files <- pd_to_file(pd)
        #print(files)
        raster_avg <- calc(stack(raster(files[1]), raster(files[2]), raster(files[3])),fun=mean)
        # Crop raster to the extent of the polygon to reduce computation
        r_crop <- crop(raster_avg, extent(cell_poly))
        # Mask the cropped raster with the polygon to extract values within the polygon area
        r_mask <- mask(r_crop, cell_poly)
        # Calculate the average value within the polygon
        avg_value <- cellStats(r_mask, stat='mean')
        return(avg_value)
    }

    uniq$value <- rep(NA, length(uniq$cell_poly))
    polys = as.list(uniq$cell_poly)
    print(paste0("Number of cell-year/season combos to get Prism data for: ",length(polys)))
    for(i in 1:length(polys)){
        print(i)
        poly <- polys[i][[1]]
        year <- uniq$year[i]
        season <- uniq$season[i]
        uniq$value[i] <- getYearSeasonPrismData(year=year,season=season,cell_poly = poly,variable=variable)
    }

    # normally - feed the year column in
    #if(is.null(avg_of_years)){
    #    uniq <- uniq %>% rowwise() %>%
    #        mutate(value=getYearSeasonPrismData(year,season,poly))
    #}
    #else{ # if averaging across years - give the same set of years to avg across for every call
    #    uniq <- uniq %>% rowwise() %>%
    #        mutate(value=getYearSeasonPrismData(avg_of_years,season,poly))
    #}

    uniq <- uniq %>% ungroup()
    # then bind it back to the original data
    bound <- df %>%
        left_join(uniq %>% dplyr::select(season, year, cell, value), by = c("season", "year", "cell"))

    bound <- bound %>% rename(new_var_colname = value)
    return(bound)
}
