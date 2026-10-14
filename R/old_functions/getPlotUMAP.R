getPlotUMAP <- function(df, umap_columns, colour_column = NULL, n_neighbors = 15, min_dist = 0.1, metric = "euclidean") {
    library(dplyr)
    library(umap)
    library(ggplot2)

    # Remove rows with missing values in the specified columns
    complete_rows <- complete.cases(dplyr::select(df, umap_columns))
    df_complete <- df[complete_rows, ]

    for_umap <- select(df_complete, umap_columns)

    # Scale the data (UMAP works best with scaled data)
    for_umap <- scale(for_umap)

    # Run UMAP on the complete data
    umap_config <- umap.defaults
    umap_config$n_neighbors <- n_neighbors
    umap_config$min_dist <- min_dist
    umap_config$meww
    umap_result <- umap(for_umap, config = umap_config)

    # Extract UMAP embeddings and bind to the dataframe
    umap_embeddings <- as.data.frame(umap_result$layout)
    colnames(umap_embeddings) <- c("UMAP1", "UMAP2")
    df_complete <- cbind(df_complete, umap_embeddings)

    # Plot UMAP
    umap_plot <- ggplot(df_complete, aes(x = UMAP1, y = UMAP2)) +
        geom_point(aes_string(color = colour_column), alpha = 0.7) +
        theme_minimal() +
        labs(title = "UMAP Plot", x = "UMAP1", y = "UMAP2")

    # Return the dataframe with UMAP components and the UMAP plot
    return(list(df_complete = df_complete, umap_embeddings = umap_embeddings, umap_plot = umap_plot))
}
