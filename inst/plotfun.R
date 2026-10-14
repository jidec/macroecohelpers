
pglmm = readRDS("D:/GitProjects/inat-daily-activity-analysis/pglmm.rds")
data = readRDS("D:/GitProjects/inat-daily-activity-analysis/data.rds")
tree = readRDS("D:/GitProjects/inat-daily-activity-analysis/tree.rds")
response_colname = "duration"
pred_colname = "daylength"
other_pred_colnames = c("tmax","wingspan")
random_effect_colnames="species"
random_effects_formula="(1|species__)"
# this is for one predictor, but need to plot interactions somehow


trimDfToTree <- function(df,tree){
    library(ape)
    tree <- drop.tip(tree,tree$tip.label[!tree$tip.label %in% unique(df$clade)])
    df <- df[df$clade %in% tree$tip.label,]
    tree$edge.length[which(is.na(tree$edge.length))] <- 0
    return(list(df,tree))
}


plotPglmmEffect <- function(pglmm, data, tree, response_colname, pred_colname, other_pred_colnames = character(), random_effect_colnames = "species", random_effects_formula = "(1|species__)") {
    library(tidyverse)
    library(dplyr)
    library(phyr)

    # make sure the number of clades is equal between data and tree
    data_and_tree <- trimDfToTree(data, tree)
    data <- data_and_tree[[1]]
    tree <- data_and_tree[[2]]

    # Create the data frame for prediction
    pred_df <- data.frame(matrix(nrow = 7, ncol = 0))
    pred_df[[response_colname]] <- NA
    pred_df[[pred_colname]] <- -3:3
    for (pred_colname2 in other_pred_colnames) {
        pred_df[[pred_colname2]] <- rep(mean(data[[pred_colname2]], na.rm = TRUE), 7)
    }
    pred_df[[random_effect_colnames]] <- NA

    # Combine data with prediction data frame
    comb_df <- data %>%
        select(c(response_colname, pred_colname, all_of(other_pred_colnames), random_effect_colnames)) %>%
        bind_rows(pred_df)

    # Construct the model formula
    fixed_effects <- c(pred_colname, other_pred_colnames)
    formula_str <- paste(response_colname, "~", paste(fixed_effects, collapse = " + "), "+", random_effects_formula)
    formula <- as.formula(formula_str)

    # Fit the model
    pred_model <- pglmm(formula, data = comb_df, cov_ranef = list(species = tree), bayes = TRUE)

    # Get the predictions for the response
    rdf1 <- pred_model$inla.model$summary.linear.predictor[(nrow(data) + 1):nrow(comb_df), ] %>%
        mutate(!!pred_colname := -3:3)

    # Plotting
    p <- ggplot(rdf1, aes_string(x = pred_colname, y = "mean")) +
        geom_ribbon(aes_string(ymin = "`0.025quant`", ymax = "`0.975quant`"), alpha = 0.15) +
        geom_line(size = 1.05)
    return(p)
}
