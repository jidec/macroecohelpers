# df must have:
# 1. datetime column
# 2. longitude column
bindSolarTime <- function(df){
    library(solartime)
    library(lubridate)
    library(dplyr)

    # add solar time
    df$solar_time <- getSolarTimeHour(as.POSIXct(df$datetime),df$longitude)
    df <- df %>% mutate(solar_interval = cut(solar_time, breaks=24))
    levels(df$solar_interval) <- 1:24
    df$local_hour <- as.numeric(df$solar_interval)
    return(df)
}
