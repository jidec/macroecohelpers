
# df must have:
# 1. datetime column
bindSeasonIntervals <- function(df, season_intervals_per_year = 4){
    library(lubridate)
    df$yday <- yday(df$datetime)
    # 4 season intervals
    if(season_intervals_per_year == 4){
        df$season <- "1"
        df$season[df$yday >= 61 & df$yday < 153] <- "2"
        df$season[df$yday >= 153 & df$yday < 245] <- "3"
        df$season[df$yday >= 245 & df$yday < 336] <- "4"
    }

    # 8 season intervals
    if(season_intervals_per_year == 8){
        df$season <- "1"
        df$season[df$yday >= 13 & df$yday < 59] <- "2"
        df$season[df$yday >= 59 & df$yday < 106] <- "3"
        df$season[df$yday >= 106 & df$yday < 153] <- "4"
        df$season[df$yday >= 153 & df$yday < 200] <- "5"
        df$season[df$yday >= 200 & df$yday < 247] <- "6"
        df$season[df$yday >= 247 & df$yday < 294] <- "7"
        df$season[df$yday >= 294 & df$yday < 340] <- "8"
    }
    return(df)
}
