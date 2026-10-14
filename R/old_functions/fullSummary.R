#' Print a full summary of a model
#'
#' @description Print summary, vifs, and r2_nakagawa if pglmm
#' @param cellsize_km the distance between each tile to grab
#' @return nothing

fullSummary <- function(model){
    library(car)
    library(performance)

    # always print summary
    print(summary(model))

    # if standard lmer. we can get vifs
    if ("lmerMod" %in% class(model)){
        print("VIFS:")
        print(vif(model))
    }

    # if a lmer, pglmm, or glmmTMB print r2 nakagawa
    if("lmerMod" %in% class(model) | "pglmm" %in% class(model)){
        print(r2_nakagawa(model))
    }
}
