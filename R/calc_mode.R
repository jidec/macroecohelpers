#' Find the most common value in a vector
#'
#' Returns the value that occurs most often. Ties go to the value that appears
#' first in `x`.
#'
#' @param x A vector of any type.
#' @param na_rm If `TRUE` (the default), missing values are ignored. If
#'   `FALSE`, `NA` is counted like any other value and can be the result.
#'
#' @returns A length-one vector of the same type as `x`. If `x` is empty, or
#'   holds only missing values and `na_rm` is `TRUE`, the result is `NA`.
#'
#' @examples
#' calc_mode(c("forest", "meadow", "forest", NA))
#'
#' # Ties go to the value seen first
#' calc_mode(c(2, 1, 1, 2))
#'
#' calc_mode(c(NA, NA, "forest"), na_rm = FALSE)
#' @export
calc_mode <- function(x, na_rm = TRUE) {
    if (!rlang::is_bool(na_rm)) {
        cli::cli_abort("{.arg na_rm} must be `TRUE` or `FALSE`.")
    }

    if (na_rm) {
        x <- x[!is.na(x)]
    }
    if (length(x) == 0) {
        # Indexing by NA keeps the class and factor levels of `x`
        return(x[NA_integer_])
    }

    values <- unique(x)
    values[which.max(tabulate(match(x, values)))]
}
