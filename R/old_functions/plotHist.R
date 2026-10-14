#' Plot a "tabled" or "untabled" hist
#'
#' @description Compute correlation then plot scatter
#' @param df dataframe
#' @param colname name of the col to plot a hist of
#' @param table whether or not to table() the variable
#' @return nothing

plotHist <- function(df, colname, table=FALSE, title=NULL,filename=NULL){
    library(ggplot2)
    library(dplyr)

    # if no title given, generate one
    if(is.null(title)){
        title <- paste("Histogram of",colname)
    }
    # if no filename given, generate one
    if(is.null(filename)){
        filename <- paste0(colname,"_hist")
    }

    if(table){
        t <- table(select(df,colname))
        hist(t, breaks=10,main="",xlab=paste("Number of observations assigned to",colname,"value"))
        print(paste("Percent of values with 3 or less observations:", sum(t <= 3) / (sum(t > 3) + sum(t <= 3))))
    }
    else{
        # save plot
        #png(paste0("saved/plots/",filename,".png"))
        p <- ggplot(data = df, aes(x = .data[[colname]])) +
            geom_histogram()
        plot(p)
        #dev.off()
        # replot in plots panel
        #plot(p)
    }
}

#data(iris)
#plotHist(iris,colname='Sepal.Length',table=FALSE)
#dev.copy(device = png, file = paste0("saved/plots/plot.png"))
#dev.off()
