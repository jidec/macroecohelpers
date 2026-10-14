#' Apply descriptive stats and visualizations to columns in a df
#'
#' @description Test for normality with plots, plot histograms, plot correlations,
#' and plot a scatter of three numerics if specified.
#' Columns must be ONLY numerics or ONLY factors, not both
#' @param df dataframe
#' @param colnames
#' @param factors specify if columns are factors
#' @param scatter_xyz_names
#'
#' @return nothing
#'
describeCols <- function(df,colnames,factors=FALSE,scatter_xyz_names=NULL,qq_plot=F,corrplot_only=F){
    library(dplyr)
    library(car)
    library(plotly)
    if(!corrplot_only){
        for(i in 1:length(colnames)){
            name <- colnames[i]
            print(name)
            vec <- as.vector(unlist(df[name]))
            if(factors){
                barplot(table(df[,name]))
                plotHist(df,colname = name,table=T)
            }
            else{
                print(summary(vec))
                if(qq_plot){
                    qqPlot(vec,ylab=name)
                }

                #plotToFile(qq,name)
                #plot(qq)
                #sh <- sample(vec, 4999)
                #print(shapiro.test(sh))
                plotHist(df,colname = name)
            }
            print("")
        }
    }
    if(!factors){
        library(corrplot)
        #png(paste0("plots/","corrplot",".png"), width = 350, height = 350)
        corrplot(cor(select(df,colnames),use="complete.obs"))
        #dev.off()
        #corrplot(cor(select(df,colnames)))
    }
    if(!is.null(scatter_xyz_names)){
        fig <- plot_ly(x=df[scatter_xyz_names[1]], y=df[scatter_xyz_names[2]], z=df[scatter_xyz_names[3]],
                       type="scatter3d", mode="markers", color=wings$Family,colors=getPalette(50))
        axx <- list(title = scatter_xyz_names[1])
        axy <- list(title = scatter_xyz_names[2])
        axz <- list(title = scatter_xyz_names[3])
        fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
    }
}

# only works for ggplots and base plots
plotToFile <- function(plot,name){
    png(paste0(name,".png"), width = 350, height = 350)
    plot(plot)
    dev.off()
    dev.new()
}

# data(iris)
# iris$BigSepalL <- as.factor(iris$Sepal.Length > mean(iris$Sepal.Length))
# iris$BigSepalW <- as.factor(iris$Sepal.Width > mean(iris$Sepal.Width))

# describeCols(iris,colnames = c("Sepal.Length","Sepal.Width"))
# describeCols(iris,colnames = c("BigSepalL","BigSepalW"),factors = TRUE)
