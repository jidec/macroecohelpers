# run a PCA, plot it, and bind it to the dataframe
# returns a tuple of the DF with PCs bound at [[1]], the rotation at [[2]] and the loadings at [[3]]
# pca_columns is a vector
getPlotBindPCA <- function(df, pca_columns, colour_column = NULLw) {
    library(dplyr)
    library(ggfortify)

    # Scale the data frame and select columns
    #df <- scaleDf(df, all_numerics = TRUE)

    # Remove rows with missing values in the specified columns
    complete_rows <- complete.cases(dplyr::select(df, pca_columns))
    df_complete <- df[complete_rows, ]

    for_pca <- select(df_complete, pca_columns)
    # Run PCA on the complete data
    pca <- prcomp(for_pca,center=TRUE,scale.=TRUE)

    # Print PCA summary and rotation matrix
    #print(summary(pca))
    rotation <- pca$rotation
    #print("Rotation:")
    #print(rotation)

    # Bind PCA results to the complete data
    df_complete <- cbind(df_complete, pca$x)

    p <- plotPCAScree(pca)
    plot(p)

    # Plot PCA
    #autoplot(pca, data = df_complete, colour = colour_column, loadings = TRUE, loadings.label = TRUE)


    # Return the data frame with PCA components, the rotation matrix, and the loadings
    return(list(df_complete, rotation, pca$loadings,pca,p))
}

#data(iris)
#iris2 <- runPlotBindPCA(iris,c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),colour_column = 'Species')
#head(iris)


