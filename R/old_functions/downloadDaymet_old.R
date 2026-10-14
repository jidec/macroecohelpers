#' Download USA Daymet daily climate data to data folder
#'
#' @description Doesn't download all data for all tiles, but rather tiles across a grid
#' with size specified by cellsize_km
#' @param cellsize_km the distance between each tile to grab
#' @return nothing

# currently 2020-21
# some options could be: what years to download, whether to summarize the years or keep them separate
downloadDaymet_old <- function(cellsize_km,download_path,return=F){
    # prep inputs, download daymet data and merge the results into daymet.csv
    library(daymetr)
    library(dplyr)
    library(sp)

    # get only contiguous US tiles
    usa_tiles <- tile_outlines # from daymetr
    usa_tiles <- cbind(usa_tiles$TileID,usa_tiles$XMin,usa_tiles$XMax,usa_tiles$YMin,usa_tiles$Ymax)
    colnames(usa_tiles) <- c("id", "XMin","XMax", "YMin","YMax")
    usa_tiles <- as.data.frame(usa_tiles)
    #library(dplyr)
    usa_tiles <- filter(usa_tiles, YMin > 24.396 & YMin < 49.38) #& YMax < -49.384 & XMin > -124.848 &)
    usa_tiles <- filter(usa_tiles, XMin > -124.84 & XMin < -66.88)
    tile_ids <- usa_tiles$id

    #nrow(filter(usa_ants, latitude < 24.396 | latitude > 49.38 | longitude < -124.84 | longitude > -66.88))

    # set lat and long to the center of each tile
    usa_tiles$X <- (usa_tiles$XMax + usa_tiles$XMin) / 2
    usa_tiles$Y <- (usa_tiles$YMax + usa_tiles$YMin) / 2
    coordinates(usa_tiles) <- cbind(usa_tiles$X,usa_tiles$Y)

    # create SpatialGridDataFrame from the usa_tiles
    grid <- makegrid(usa_tiles, cellsize = cellsize_km / 110.574) #cellsize_miles / 69
    grid$id <- 1:nrow(grid)
    coordinates(grid) <- cbind(grid$x1,grid$x2)
    gridded(grid) <- TRUE
    grid <- as(grid, "SpatialGridDataFrame")

    # write to .csv to feed into daymet func
    points <- cbind(grid$id,grid$x2,grid$x1)
    colnames(points) <- c("site","latitude","longitude")
    points <- as.matrix(points)
    points_path <- paste0(download_path,"/points_for_daymet.csv")
    write.csv(points, points_path, row.names = FALSE)

    data_path <- paste0(download_path,"/daymet")
    # make folder
    dir.create(data_path)

    # download 16500 (max) tiles from these points
    download_daymet_batch(
        file_location = points_path,
        start = 2020,
        end = 2021,
        internal = FALSE,
        path = "data/daymet",
        simplify = FALSE
    )

    csv_files <- dir(path= data_path, pattern='*.csv$', recursive = T)
    csv_files <- paste0(data_path, "/", csv_files)

    daymet_data <- data.frame()
    for(i in 1:length(csv_files)) {
        daymet_data <- rbind(daymet_data, read_daymet(csv_files[i]))
    }

    # season starts
    #install.packages("lubridate")
    #library(lubridate)
    #yday(mdy("12/1/2000"))
    #yday(mdy("3/1/2000"))
    #yday(mdy("6/1/2000"))
    #yday(mdy("9/1/2000"))

    data_file_path <- paste0(data_path, "/daymet.csv")
    write.csv(daymet_data,data_file_path)

    if(return){
        return(daymet_data)
    }
}

#cellsize_km=500
#download_path="D:/GitProjects/inat-daily-activity-analysis/data"
#return=T
#library(daymetr)
#data <- downloadDaymet(cellsize_km=250, download_path="D:/GitProjects/inat-daily-activity-analysis/data",return=T)
