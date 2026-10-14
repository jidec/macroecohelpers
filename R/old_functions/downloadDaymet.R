

# this function is used to assemble daymet data given a set of lat lon locations (cells)
# gridded_df must have cell, cell_lat, and cell_lon, representing either a grid cell or a targeted site (like a city with coordinate)
downloadDaymet <- function(gridded_df, download_path, return=F){

    library(daymetr)
    library(dplyr)
    library(sp)
    library(stringr)

    # get unique grid cells from the data
    grid_cells <- gridded_df %>%
        select(c(cell, cell_lat, cell_lon)) %>%
        distinct(cell, .keep_all = TRUE)

    # write to .csv to feed into daymet func
    colnames(grid_cells) <- c("site","latitude","longitude")
    grid_cells <- as.matrix(grid_cells)
    grid_cells_path <- paste0(download_path,"/points_for_daymet.csv")
    write.csv(grid_cells, grid_cells_path, row.names = FALSE)

    csvs_path <- paste0(download_path,"/daymet")
    # make folder
    dir.create(csvs_path)

    # download 16500 (max) tiles from these points
    download_daymet_batch(
        file_location = grid_cells_path,
        start = 2015,
        end = 2023,
        internal = FALSE,
        path = paste0(download_path,"/daymet"),
        simplify = FALSE
    )

    csv_files <- dir(path= csvs_path, pattern='*.csv$', recursive = T)
    csv_files <- paste0(csvs_path, "/", csv_files)

    daymet_data <- data.frame()
    for(i in 1:length(csv_files)) {

        # get full name of the csv
        csv_name <- str_split(csv_files[i],"/")[[1]][length(str_split(csv_files[i],"/")[[1]])]
        cell_id <- str_split(csv_name,"_")[[1]][1] # get the id as the first element of split on _

        new_data <- read_daymet(csv_files[i])
        new_data$cell <- rep(cell_id, nrow(new_data))
        daymet_data <- rbind(daymet_data, new_data)
    }

    data_file_path <- paste0(download_path, "/daymet.csv")
    write.csv(daymet_data,data_file_path)


    if(return){
        return(daymet_data)
    }
}
