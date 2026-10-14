#' Share of non-missing values at each level, as a one-row tibble
#' @noRd
calc_proportions <- function(x) {
    tibble::as_tibble(as.list(prop.table(table(x))))
}
