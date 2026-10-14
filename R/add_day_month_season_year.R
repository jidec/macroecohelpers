#' Add the day of year, month, season, and year of each observation
#'
#' Adds four columns taken from a datetime column: `day`, the day of the year
#' (1 to 366); `month` (1 to 12); `season`, the meteorological season; and
#' `year`. All four are read in the time zone the datetime is stored in.
#'
#' Seasons are named as in the northern hemisphere: `"winter"` is December
#' to February, `"spring"` March to May, `"summer"` June to August, and
#' `"fall"` September to November.
#'
#' @param data A data frame.
#' @param datetime <[`data-masking`][dplyr::dplyr_data_masking]> Column of
#'   observation times, as POSIXct or Date.
#'
#' @returns A tibble with the same rows as `data` and added `day`, `month`,
#'   `season`, and `year` columns. Rows with a missing datetime get `NA`.
#'
#' @examples
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "A. alba", "B. nigra"),
#'     observed_at = as.POSIXct(
#'         c("2021-01-15 09:00:00", "2021-06-01 14:30:00", "2020-12-31 17:00:00"),
#'         tz = "UTC"
#'     )
#' )
#'
#' add_day_month_season_year(occurrences, datetime = observed_at)
#' @importFrom rlang .data
#' @export
add_day_month_season_year <- function(data, datetime = datetime) {
    # Seasons in calendar order from December, which starts winter
    seasons <- c("winter", "spring", "summer", "fall")

    data |>
        dplyr::mutate(
            # Day of the year, not of the month
            day = as.integer(lubridate::yday({{ datetime }})),
            month = as.integer(lubridate::month({{ datetime }})),
            # Months 12, 1, 2 fall in the first season, 3 to 5 in the second, and so on
            season = seasons[(.data$month %/% 3) %% 4 + 1],
            year = as.integer(lubridate::year({{ datetime }}))
        ) |>
        tibble::as_tibble()
}
