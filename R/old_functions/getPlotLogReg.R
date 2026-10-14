#' Create and plot a logistic regression model
#'
#' @description
#' @param df to use columns from
#' @param response name of response column
#' @param formula string containing formula i.e. lat + temp + precip
#' @return the model

getPlotLogReg  <- function(df, response, formula){
    library(rsq)
    library(sjPlot)
    library(pROC)
    library(dplyr)

    df <- scaleDf(df,all_numerics = TRUE)

    df$id <- 1:nrow(df)
    train <- df %>% dplyr::sample_frac(0.80)
    test  <- dplyr::anti_join(df, train, by = 'id')

    m <- eval(parse(text=paste0("glm(",response,"~",formula,", data = train, family=binomial)")))
    print(summary(m))
    #print("RSQ:")
    #print(rsq(m))

    # plot the coefficients and their confidence intervals
    plot(plot_model(m))
    #plot(plot_model(m, type = "est", vline.color = "gray"))
    print(test)
    test$pred <- predict(m, test, type="response")
    test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
    print(test)
    print(confusionMatrix(test$good_pred, test$good))

    #test_prob = predict(m, newdata = test, type = "response")
    #test_roc = roc(test[,response] ~ test_prob, plot = TRUE, print.auc = TRUE)

    # predict response probabilities for the training data
    #train_prob <- predict(m, type = "response")

    #length(train_prob)
    #length(df[,response])

    # create a ROC curve object
    #roc_data <- roc(df[,response], train_prob)

    # plot the ROC curve
    #plot(roc_data, print.auc = TRUE, legacy.axes = TRUE)

    return(m)
}

# data(iris)
# iris$BigSepalL <- as.factor(iris$Sepal.Length > mean(iris$Sepal.Length))
# iris$BigSepalW <- as.factor(iris$Sepal.Width > mean(iris$Sepal.Width))
# df <- iris
# response <- "BigSepalL"
# formula <- "Sepal.Width + Petal.Length + Petal.Width"

