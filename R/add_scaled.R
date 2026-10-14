#' Add scaled copies of numeric columns
#'
#' Adds a `{column}_sc` column for each selected column, centred on its mean
#' and divided by its standard deviation. Missing values are ignored when
#' computing the mean and standard deviation and stay missing. The original
#' columns are left unchanged.
#'
#' @param data A data frame.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to scale.
#'   Defaults to every numeric column.
#'
#' @returns A tibble with the same rows as `data` and one new `{column}_sc`
#'   column per scaled column.
#'
#' @examples
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "A. alba", "B. nigra", "B. nigra"),
#'     latitude = c(29.6, 30.1, 44.0, 45.2),
#'     elevation = c(12, 40, 850, NA)
#' )
#'
#' add_scaled(occurrences)
#'
#' add_scaled(occurrences, cols = latitude)
#' @export
add_scaled <- function(data, cols = dplyr::where(is.numeric)) {
    # Centre on the mean and divide by the standard deviation
    scale_values <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)

    data |>
        # Scaled copies go alongside the originals as {column}_sc
        dplyr::mutate(dplyr::across({{ cols }}, scale_values, .names = "{.col}_sc")) |>
        tibble::as_tibble()
}
