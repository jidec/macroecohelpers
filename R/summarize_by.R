#' Summarize every column by group, with a default chosen by column type
#'
#' Collapses `data` to one row per group. Numeric columns become their mean
#' and logical columns the proportion `TRUE`. A character or factor column
#' with between 2 and `max_levels` distinct values becomes one column per
#' value, named `{column}_{value}`, holding the proportion of the group with
#' that value. Any other column becomes its most common value (see
#' [calc_mode()]). Missing values are ignored, and so are blank strings in
#' the columns expanded into proportions.
#'
#' @param data A data frame.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to group by.
#' @param ... Name-value pairs passed to [dplyr::summarise()]. Naming an
#'   existing column replaces its default summary; a new name adds a column.
#' @param max_levels The most distinct values a character or factor column
#'   can have and still be expanded into proportions.
#'
#' @returns A tibble with one row per group: the `by` columns, the columns
#'   from `...`, the proportion columns, the remaining columns, and the group
#'   size `n`.
#'
#' @examples
#' occurrences <- tibble::tibble(
#'     species = c("A. alba", "A. alba", "A. alba", "B. nigra", "B. nigra"),
#'     family = "Pieridae",
#'     sex = c("female", "male", "male", "female", NA),
#'     latitude = c(29.6, 30.1, 31.4, 44.0, 45.2),
#'     year = c(2019, 2021, 2020, 2018, 2022),
#'     is_captive = c(FALSE, FALSE, TRUE, FALSE, FALSE)
#' )
#'
#' summarize_by(occurrences, by = species)
#'
#' summarize_by(
#'     occurrences,
#'     by = species,
#'     year = min(year),
#'     sex = calc_mode(sex)
#' )
#' @export
summarize_by <- function(data, by, ..., max_levels = 5) {
    # Columns the caller summarises themselves in `...`
    custom <- names(rlang::enquos(...))

    # Categorical columns with few enough values to expand into proportions
    has_few_levels <- function(x) {
        (is.character(x) || is.factor(x)) &&
            dplyr::n_distinct(x[x != ""], na.rm = TRUE) %in% 2:max_levels
    }
    expand <- data |>
        dplyr::select(dplyr::where(has_few_levels) & !c({{ by }}, dplyr::any_of(custom))) |>
        names()

    # Default for every other column: mean, or most common value
    default <- function(x) {
        if (is.numeric(x) || is.logical(x)) mean(x, na.rm = TRUE) else calc_mode(x)
    }

    data |>
        # As factors, every group reports the same set of levels; blanks are missing
        dplyr::mutate(dplyr::across(dplyr::all_of(expand), \(x) factor(x, exclude = c(NA, "")))) |>
        dplyr::summarise(
            # Caller's summaries go first so they see the raw columns
            ...,
            # One proportion column per level
            dplyr::across(dplyr::all_of(expand), calc_proportions, .unpack = "{outer}_{inner}"),
            # Type default for the rest
            dplyr::across(!dplyr::any_of(c(custom, expand)), default),
            # Group size
            n = dplyr::n(),
            .by = {{ by }}
        ) |>
        tibble::as_tibble()
}
