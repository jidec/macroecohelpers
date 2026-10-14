#' Scale specific columns (or all numeric cols) in a dataframe
#'
#' @param df dataframe
#' @param colnames character vector of column names to scale
#' @param all_numerics whether to just scale all the numeric cols
#' @return scaled df with new columns containing scaled values
#'
scaleDf <- function(df, colnames = NULL, all_numerics = FALSE) {
    library(dplyr)

    if(all_numerics) {
        # scale all numeric columns
        numeric_cols <- df %>% select(where(is.numeric))
        scaled_cols <- numeric_cols %>% mutate(across(everything(), ~ scale(.)[, 1]))
        colnames(scaled_cols) <- paste0(colnames(scaled_cols), "_sc")
        df <- bind_cols(df, scaled_cols)
    } else if (!is.null(colnames)) {
        # scale specified columns
        scaled_cols <- df %>% select(all_of(colnames)) %>% mutate(across(everything(), ~ scale(.)[, 1]))
        colnames(scaled_cols) <- paste0(colnames(scaled_cols), "_sc")
        df <- bind_cols(df, scaled_cols)
    }

    return(df)
}
