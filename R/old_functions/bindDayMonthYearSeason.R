
# d must have "datetime" column
bindDayMonthYearSeason <- function(d) {
    d <- bindSeasonIntervals(d,season_intervals_per_year = 4)

    # Extracting month and year from a date column (assuming the date column is named 'date')
    d <- d %>%
        mutate(
            month = lubridate::month(datetime),
            year = lubridate::year(datetime),
        )

    return(d)
}
