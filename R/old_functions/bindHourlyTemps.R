
# df must have columns:
# 1. yday (added with lubridate)
# 2. latitude
# 3. year
# 4. tmin
# 5. tmax
# 6. id (used for binding)
# 7. local_hour
bindHourlyTemps <- function(data){

    library(chillR)
    library(dplyr)
    library(lubridate)

    # read in
    #data <- read.csv("serena_temps/data_w_temp.csv")

    # add month, day of month, and latitude columns
    dates <- as.Date(data$yday - 1, origin = "2021-01-01")
    month_column <- month(dates)
    day_of_month_column <- day(dates)
    data$Month <- month_column
    data$Day <- day_of_month_column
    data$Latitude <- round(data$latitude)

    # save unique latitudes
    uniq_lats <- unique(data$Latitude)

    # subset and rename data for stack hourly temps
    data_for_hourly <- data %>% select(c(year,tmin,tmax,Month,Day,Latitude,id))
    colnames(data_for_hourly) <- c("Year","Tmin","Tmax","Month","Day","Latitude","ID")

    hourly_temp_data <- data.frame()
    for(lat in uniq_lats){
        new_data <- dplyr::filter(data_for_hourly,Latitude==lat)
        ids <- new_data$ID
        new_data <- new_data[,1:5]
        new_data <- stack_hourly_temps(new_data, latitude=lat)$hourtemps
        new_data$ID <- ids
        hourly_temp_data <- rbind(hourly_temp_data,new_data)
    }

    hourly_temp_data <- hourly_temp_data %>% select(c(Temp,ID,Hour))

    colnames(hourly_temp_data) <- c("hour_temperature","id","local_hour")

    # final data needs to ONLY bind on the hour temperature of the hour for a given id

    final_data <- merge(data, data_for_hourly, by=c("id","local_hour"))
    return(final_data)
}
