#' Given a model response and formula, step an LM and print the VIFs to aid in model selection of the best formula
#'
#' @description
#' @param df to use columns from
#' @param response name of response column
#' @param formula string containing formula i.e. lat + temp + precip
#' @return the model

vifsAndStep <- function(df, response, formula,mixed=TRUE){
    library(car)
    # if mixed, do a "lmer" instead of an "lm"
    mixed_str <- ""
    if(mixed){
        library(lme4)
        mixed_str <- "er"
    }
    # build the model
    model <- eval(parse(text=paste0("lm",mixed_str,"(",response,"~",formula,", data = df)")))

    # print vifs, model summary, and step for model selection by AIC
    print(summary(model))
    print(step(model))
    print("VIFS:")
    print(vif(model,type='predictor'))

    if(any(vif(model) > 5)){
        print("Warning: some VIFs above 5, call vifsAndStep again with offenders removed from formula")
    }
}

#data(iris)
#vifsAndStep(iris,response="Sepal.Length",formula="Sepal.Width + Petal.Length + Petal.Width",mixed=FALSE)
#vifsAndStep(iris,response="Sepal.Length",formula="Sepal.Width + Petal.Length",mixed=FALSE)

