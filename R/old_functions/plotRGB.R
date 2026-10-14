#' @title plotRGB
#'
#' @description method to quickly visualize an RGB color
#'
#' @param rgb a numeric vector of length 3 of rgbs scaled from 0-255
#'
#' @rdname plotRGBColor
#' @export
#'

# plot an RGB color square in R - use to quickly visualize colors
# or place in plots/powerpoints
plotRGB <- function(rgb,max_val=1) {
    library(grDevices)
    # todo - finish allowing matrix of multiple rgbs
    #rgb <- rbind(rgb, rgb2)
    #rgb[,1], rgb[,2]
    col <- grDevices::rgb(rgb[1], rgb[2], rgb[3], maxColorValue = max_val)
    return(plot(c(1), col = col, pch = 15, cex = 40, axes = FALSE, ylab='',xlab=''))
}
