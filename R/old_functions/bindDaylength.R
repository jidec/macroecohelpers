
# df must have:
# !. latitude and longitude cols
# 2. either yday or season ("1","2","3","4") col
##df = readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/sscs2_traits")
#use_season_median = TRUE

bindDaylength <- function(df,use_season_median=TRUE){
    library(suncalc)
    library(dplyr)
    library(lubridate)
    if(use_season_median){
        df$date <- rep(NA,nrow(df))
        df$date[df$season == "1"] <- "2023-12-21"
        df$date[df$season == "2"] <- "2023-3-20"
        df$date[df$season == "3"] <- "2023-6-21"
        df$date[df$season == "4"] <- "2023-9-22"
        df$date <- as.Date(df$date)
    }
    df$lon <- df$longitude
    df$lat <- df$latitude

    times <- getSunlightTimes(data=df, tz = "CET")
    df$daylength <- times$sunset - times$sunrise
    df <- df %>% dplyr::select(-date)
    return(df)
}

alternateDayLengthCalc <- function(yday, latitude) {
    # Constants
    deg2rad <- pi / 180
    rad2deg <- 180 / pi

    # Convert latitude to radians
    latitudeRad <- latitude * deg2rad

    # Solar declination
    declination <- 23.45 * sin(deg2rad * (360 / 365) * (yday - 81))
    declinationRad <- declination * deg2rad

    # Calculate the hour angle at sunrise/sunset
    cosHourAngle <- -tan(latitudeRad) * tan(declinationRad)

    # Ensure the value is between -1 and 1 to avoid NaN results
    cosHourAngle <- ifelse(cosHourAngle > 1, 1, ifelse(cosHourAngle < -1, -1, cosHourAngle))

    hourAngle <- acos(cosHourAngle)

    # Day length in hours
    dayLengthHours <- 2 * hourAngle * rad2deg / 15

    return(dayLengthHours)
}
#library(suncalc)
#times <- getSunlightTimes(lat = 48.03854,lon=-123.6079,date=as.Date("2023-9-22"), tz = "CET")
#dl <- times$sunset - times$sunrise
#dl
