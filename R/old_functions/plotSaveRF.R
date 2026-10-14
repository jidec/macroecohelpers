
plotSaveRF <- function(df, response, formula){

    library(randomForest)
    library(randomForestExplainer)
    library(dplyr)
    df <- dplyr::filter(df, !is.na(response))

    # Split the dataset into training and testing sets
    train_indices <- sample(nrow(df), 0.8 * nrow(df))
    train_data <- df[train_indices, ]
    test_data <- df[-train_indices, ]

    # train RF
    m <- eval(parse(text=paste0("randomForest(",response,"~",formula,", data = train_data, importance=TRUE,na.action=na.omit)"))) # create model using response and formula

    # Make predictions on the testing set
    predictions <- predict(m, test_data)

    # Evaluate the accuracy of the predictions
    confusion_matrix <- table(predictions, test_data$flight_type)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

    # Print the confusion matrix and accuracy
    print(confusion_matrix)
    print(paste0("Accuracy: ", accuracy))

    # plot decision trees
    #tree_data <- getTree(m, k = 1)
    #plot_tree(tree_data)

    # plot variable importance
    plot(varImpPlot(m))
}
