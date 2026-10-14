
getPlotBRMS <- function(response, formula, tree, data, family=gaussian(),add_spatial_effect=FALSE, save_path=NA) {
    library(brms)
    library(rlang)
    library(performance)
    library(bayestestR)

    # combine the provided formula with the phylo effect and sometimes the spatial effect
    if(add_spatial_effect){
        full_formula <- as.formula(paste(response, "~", formula, "+ (1|gr(spp, cov = tree_cov)) + t2(latRound, lonRound)"))
    }
    else{
        full_formula <- as.formula(paste(response, "~", formula, "+ (1|gr(spp, cov = tree_cov))"))
    }

    tree_cov <- ape::vcv.phylo(tree)
    data$spp <- data$species
    # Define the tree_cov data as a named list
    data2 <- list(tree_cov = tree_cov)

    # if adding effect for spatial autocorrelation
    if(add_spatial_effect){
        data <- data %>%
            mutate(latRound = round(latitude,2),
                   lonRound = round(longitude,2))
    }

    # Fit the brms model
    m <- brm(
        formula = full_formula,
        data = data,
        data2 = data2,
        family = family,
        chains = 3,
        cores = 3,
        iter = 3000,
        warmup = 1000
    )

    summary(m)

    # r2
    #r2_bayes(m)
    #bayes_R2(m)

    # measure of chain convergence
    #rhat(m)

    #rstan::Rhat(m)

    # measure of mixing - number of independent samples that the chains have used
    #effective_sample(m)

    # generates predictive distributions from the posterior samples and compares them to the observed data
    # visualizes discrepancies between predictions and data
    #pp_check(m)

    # leave-one-out cross-validation
    # measures performance of model on new data
    #loo(m)

    if(!is.na(save_path)){
        saveRDS(m,save_path)
    }

    return(m)
}
