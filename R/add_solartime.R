#' Add the solar time of each observation
#'
#' Adds `solar_time`, the time of day by the sun at each observation's
#' longitude, in hours from 0 up to 24. The sun is highest at 12, wherever
#' and whenever the observation was made, so solar time compares time of day
#' across longitudes and seasons better than clock time does. Computed with
#' [solartime::getSolarTimeHour()].
#'
#' @param data A data frame.
#' @param datetime <[`data-masking`][dplyr::dplyr_data_masking]> Column of
#'   observation times, as POSIXct. Any time zone works; the instant is what
#'   matters.
#' @param lon <[`data-masking`][dplyr::dplyr_data_masking]> Column holding
#'   longitude in decimal degrees.
#'
#' @returns A tibble with the same rows as `data` and an added `solar_time`
#'   column. Rows with a missing time or longitude get `NA`.
#'
#' @examplesIf rlang::is_installed("solartime")
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "B. nigra"),
#'     observed_at = as.POSIXct(c("2021-06-01 17:30:00", "2021-06-01 17:30:00"), tz = "UTC"),
#'     decimalLongitude = c(-82.32, -104.99)
#' )
#'
#' add_solartime(occurrences, datetime = observed_at, lon = decimalLongitude)
#' @export
add_solartime <- function(data, datetime = datetime, lon = longitude) {
    rlang::check_installed("solartime")

    # In UTC, as solartime mishandles zones offset by a half hour
    utc <- lubridate::with_tz(dplyr::pull(data, {{ datetime }}), "UTC")

    data |>
        # Wrapped into one day, as times near midnight come back below 0 or above 24
        dplyr::mutate(solar_time = solartime::getSolarTimeHour(utc, {{ lon }}) %% 24) |>
        tibble::as_tibble()
}
