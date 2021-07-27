
# Testing a randomforest model!
library(randomForest)

f <- as.formula(income ~ .)

compare_trees <- function(rng){
  acc <- c()
  ntree <- c()
  mtry <- c()
  for (i in rng){
    # The standard mtry for classification trees (default) is SQRT(p),
    # Where p is the # of variables in the model
    for (j in seq(from = 2, to = 4, by = 1)){
    rf_regressor = randomForest(f,data = train, ntree = i, mtry = j)
    test$yhat <- predict(rf_regressor, test)
    acc <- append(acc, sum(test$yhat == test$Attrition) / nrow(test))
    ntree <- append(ntree, i)  
    mtry <- append(mtry, j)
  }}
  
  return(data.frame(acc = acc, ntree = ntree, mtry = mtry))
}

# Timing this run for future tests
system.time(trees <- compare_trees(50:55))

# Train the model
rf_classifier <- randomForest(f, data = train, 
                             ntree = trees[which.max(trees$acc), ]$ntree,
                             mtry = trees[which.max(trees$acc), ]$mtry)

# Predict and plot the predictions
rf_pred <- predict(rf_classifier, test)

# Showing the confusion Matrix 
confusionMatrix(table(rf_pred, test$income))

# Cross validation of our dataset .How does it compare with our one-hoc RMSE?
library(rfUtilities)
rf.crossValidation(x = rf_classifier, xdata = train, 
                   n = 10, bootstrap = TRUE,
                   seed = 2021)

# Accuracy appending
# acc_df <- append(acc_df, round(sum(rf_pred == test$income) / length(test$income),2)*100 )
# model_df <- append(model_df, "Random Forest")

# Showing the confusion Matrix 
paste0("Confusion Matrix of Attrition Classification")
confusionMatrix(table(rf_pred, test$income))
