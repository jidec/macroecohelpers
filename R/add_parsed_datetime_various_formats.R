#' Add a datetime column parsed from dates written in various formats
#'
#' Parses a column of date strings whose format varies from row to row, as in
#' occurrence records pooled from several sources, and adds the result as
#' `datetime`. Handles dates with or without a time, `-` or `/` separators,
#' 24-hour or AM/PM times, ISO 8601 strings, and strings like
#' `"Tue Jun 01 2021 14:30:00 GMT-0400 (EDT)"`.
#'
#' Numeric UTC offsets such as `-0400` are applied. Time zone abbreviations
#' such as `EDT` are ignored, and those times are read as UTC.
#'
#' @param data A data frame.
#' @param date <[`data-masking`][dplyr::dplyr_data_masking]> Column of date
#'   strings to parse.
#'
#' @returns `data` as a tibble with an added POSIXct column `datetime`, in
#'   UTC. Dates that match no format are `NA`.
#'
#' @examples
#' occurrences <- tibble::tibble(
#'     observed_on = c(
#'         "2021-06-01 14:30:00",
#'         "2021/06/01 2:30 PM",
#'         "Tue Jun 01 2021 14:30:00 GMT-0400 (EDT)",
#'         "2021-06-01"
#'     )
#' )
#'
#' add_parsed_datetime_various_formats(occurrences, date = observed_on)
#' @export
add_parsed_datetime_various_formats <- function(d, date = date) {
    # Formats to try on each date
    orders <- c(
        "Y-m-d H:M:S z", "Y-m-d H:M:S", "Y-m-d",
        "Y/m/d H:M:S p z", "Y/m/d H:M:S p", "Y/m/d H:M p z", "Y/m/d H:M p",
        "Y/m/d H:M:S", "Y/m/d H:M", "Y/m/d",
        "a b d Y H:M:S z", "a b d Y H:M:S"
    )

    d |>
        # Dates that match no format become NA
        dplyr::mutate(
            datetime = lubridate::parse_date_time({{ date }}, orders = orders, quiet = TRUE)
        ) |>
        tibble::as_tibble()
}
