#' Create and plot a lmer or pglmm
#'
#' @description
#' @param df to use columns from
#' @param response name of response column
#' @param formula string containing formula i.e. lat + temp + precip
#' @return the model

getPlotLM <- function(df,model_type="lmer",response,formula,scale=FALSE,plotmodel_terms=c(),save_name=NULL,
                      tree=NULL,replace_underscores=T,remove_commas=T,remove_first_split=F,
                      log_transf_colnames=NULL,bayes=T){
    library(phyr)
    library(lme4)
    library(dplyr)
    library(sjPlot)
    library(glmmTMB)

    # log transform
    if(!is.null(log_transf_colnames)){
        for(c in log_transf_colnames){
            df[,c] <- log(df[,c])
        }
    }
    # scale all numerics in df EXCEPT response
    if(scale){
        resp_col <- df[,response]
        df <- scaleDf(df,all_numerics = TRUE)
        df[,response] <- resp_col
    }

    if(model_type == "lmer"){
        m <- eval(parse(text=paste0("lmer(",response,"~",formula,", data = df)"))) # create model using response and formula
        plot(plot_model(m)) # plot it
        for(t in plotmodel_terms){ # plot it by each of its terms
            plot(plot_model(m,type="pred",terms=t))
        }
    }
    if(model_type == "pglmm"){
        trimmed_tuple <- trimDfToTree(df,tree)
        df <- trimmed_tuple[[1]]
        tree <- trimmed_tuple[[2]]
        bayes_str <- as.character(bayes)
        m <- eval(parse(text=paste0("pglmm(",response,"~",formula,", data = df, cov_ranef = list(species=tree), bayes=",bayes_str,")")))
        if(bayes){
            plot(plot_bayes(m))
        }
        else{
            #library(phyr)
            #plot(pglmm_plot_ranef(x=m))
            #plot(plot_model(m))
        }
    }
    if(model_type == "glmmTMB_beta_zi"){
        m <- eval(parse(text=paste0("glmmTMB(",response,"~",formula,", data = df, family=beta_family(link=\"logit\"),  ziformula = ~.)"))) # create model using response and formula
    }
    if(model_type == "glmmTMB_beta"){
        m <- eval(parse(text=paste0("glmmTMB(",response,"~",formula,", data = df, family=beta_family(link=\"logit\"))"))) # create model using response and formula
    }

    # save
    #if(is.null(save_name)){
    #    save_name <- paste(deparse(substitute(df)),response,formula,".rds",sep='_')
    #}

    #if(!is.null(save_name)){
    #    saveRDS(m,file=paste0("saved/models/",save_name))
    #}
    #return(m)

    fullSummary(m)

    return(m)
}
