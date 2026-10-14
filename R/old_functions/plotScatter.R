#' Plot a scatter of two correlated variables
#'
#' @description Compute correlation then plot scatter
#' @param cellsize_km the distance between each tile to grab
#' @return nothing

plotScatter <- function(df, colname1, colname2){
    library(dplyr)
    c <- cor.test(select(df,colname1),select(df,colname2))
    print(c)
    plot(ggplot(df, aes(x=colname1, y=colname2)) + geom_point())
}
