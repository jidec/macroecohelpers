
plotPCAScree <- function(pca){
    # Calculate the proportion of variance explained by each principal component
    var_explained <- pca$sdev^2 / sum(pca$sdev^2)
    cum_var_explained <- cumsum(var_explained)

    # Limit the number of PCs to plot
    num_pcs_to_plot <- 20  # Adjust this number as needed

    # Create a data frame for plotting
    pca_df <- data.frame(
        Principal_Component = factor(1:num_pcs_to_plot),
        Variance_Explained = var_explained[1:num_pcs_to_plot],
        Cumulative_Variance_Explained = cum_var_explained[1:num_pcs_to_plot]
    )

    # Plot the proportion of variance explained by each principal component
    p <- ggplot(pca_df, aes(x = as.numeric(Principal_Component), y = Variance_Explained)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_line(aes(y = Cumulative_Variance_Explained, group = 1), color = "red", size = 1) +
        geom_point(aes(y = Cumulative_Variance_Explained), color = "red", size = 2) +
        labs(
            title = "Scree Plot",
            x = "Principal Component",
            y = "Proportion of Variance Explained"
        ) +
        theme_minimal() +
        scale_x_continuous(breaks = 1:num_pcs_to_plot)
    plot(p)
    return(p)
}
