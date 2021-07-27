library(ROCR)
noskill <- function(actuals, return_metric = "auc", print_proportion = TRUE){

majority_class <- (as.data.frame(table(actuals))[as.data.frame(table(actuals))$Freq == 
                             max(as.data.frame(table(actuals))$Freq), ])$actuals
# Print if print is true
if (print_proportion) {
print(paste0("Majority class is: ", majority_class,
" With ", round(100*(as.data.frame(table(actuals))[as.data.frame(table(actuals))$Freq == max(as.data.frame(table(actuals))$Freq), ])$Freq/sum(as.data.frame(table(actuals))$Freq),2),
"% of the sample"))
}
  # Create the no skill prediction
  noskill_pred <- c(rep(majority_class, length(actuals)))
  # No skill accuracy
  noskill_acc <- sum(noskill_pred == actuals) / length(actuals)
  # No Skill AUC
  noskill_auc <- performance(prediction(as.numeric(noskill_pred), as.numeric(actuals)), 
              "auc")@y.values[[1]]
  # Noskill_F1
  noskill_f1 <- performance(prediction(as.numeric(noskill_pred), as.numeric(actuals)), 
              "f")@y.values[[1]][2]
  
  if(return_metric == "auc"){
    return(noskill_auc)
  } 
  else if (return_metric = "f1"){
    return(noskill_f1)
  }
  else{
    return(noskill_acc)
  }
}
