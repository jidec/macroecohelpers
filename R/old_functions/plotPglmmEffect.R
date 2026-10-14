#library(colorEvoHelpers)

# pglmm = readRDS("D:/GitProjects/inat-daily-activity-analysis/pglmm.rds")
# data = readRDS("D:/GitProjects/inat-daily-activity-analysis/data.rds")
# tree = readRDS("D:/GitProjects/inat-daily-activity-analysis/tree.rds")
# response_colname = "duration"
# pred_colname = "daylength"
# other_pred_colnames = c("tmax","wingspan")
# random_effect_colnames="species"
# random_effects_formula="(1|species__)"

#p + labs(x = "Daylength", y = "Onset") + geom_point(data = sscs, aes(x = daylength_sc, y = q10_value), alpha=0.05)
plotPglmmEffect <- function(pglmm, data, tree, response_colname="", pred_colname="", other_pred_colnames="", random_effect_colnames="species", random_effects_formula="(1|species__)") {
    library(tidyverse)
    library(dplyr)
    library(phyr)

    # make sure the number of clades is equal between data and tree
    data_and_tree <- trimDfToTree(data,tree)
    data <- data_and_tree[[1]]
    tree <- data_and_tree[[2]]

    # init string to parse when creating df of values to predict using
    pred_df_parse <- "data.frame("
    pred_df_parse <- paste0(pred_df_parse, response_colname, " = rep(NA,7),")
    pred_df_parse <- paste0(pred_df_parse, pred_colname, " = -3:3,")
    # add each other predictor in our model other than the main predictor we want to plot the effect of
    for(pred_colname2 in other_pred_colnames) {
        pred_df_parse <- paste0(pred_df_parse, pred_colname2, " = rep(mean(data$", pred_colname2, ", na.rm=T), 7),")
    }
    # finally add species as NA
    pred_df_parse <- paste0(pred_df_parse, "species = rep(NA, 7))")
    # then create the df
    pred_df <- eval(parse(text = pred_df_parse))
    # this final df has:
    # 1. a column for the response with NAs
    # 2. a column for the predictor to plot the effect of that ranges from -3 to 3
    # 3. columns for the predictors with NAs
    # 4. a column "species" with NAs

    # we bind the data with this df
    comb_df <- data %>%
        select(c(response_colname, pred_colname, other_pred_colnames, random_effect_colnames)) %>% #random_effect_colnames="cell"
        bind_rows(pred_df)

    # construct the model formula using the pred and response colnames
    pred_model_parse <- paste0("pglmm(", response_colname, " ~ ", pred_colname, " + ")
    for(pred_colname2 in other_pred_colnames) {
        pred_model_parse <- paste0(pred_model_parse, pred_colname2, " + ")
    }
    pred_model_parse
    # create the model
    pred_model_parse <- paste0(pred_model_parse, random_effects_formula, ",data = comb_df, cov_ranef = list(species=tree), bayes=TRUE)")
    pred_model <- eval(parse(text = pred_model_parse))

    # get the predictions for the response
    rdf1_parse <- paste0("pred_model$inla.model$summary.linear.predictor[(nrow(data)+1):nrow(comb_df),] %>%
    mutate(", pred_colname, " = -3:3)")
    #
    rdf1 <- eval(parse(text = rdf1_parse))
    #
    p_parse <- paste0("ggplot(rdf1, mapping = aes(x = ", pred_colname, ", y = mean)) +
    geom_ribbon(mapping = aes(ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.15) +
    geom_line(mapping = aes(), linewidth = 1.05)")
    #
    p <- eval(parse(text = p_parse))

    return(p)
}
