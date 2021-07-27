library(ROCR)

optimal_cutoff <- function(probabilities, actuals, test_rng = seq(from = 0.4, to = 0.6, by = 0.01)){
  cutoff_vals <- c()
  aucs <- c()
  for (cutoff in test_rng){
    tmp_prob <- probabilities
    tmp_prob[tmp_prob > cutoff] <- 1
    # create prediction object
    aucs <- append(aucs, performance(prediction(tmp_prob, actuals), "auc")@y.values[[1]])
    cutoff_vals <- append(cutoff_vals, cutoff)
  }
  output_df <- data.frame(auc = aucs, cutoff = cutoff_vals)
  return(output_df[which.max(output_df$auc), ]$cutoff)
}

## USAGE ##
# test_over$Prediction[test_over$incomeProbability>  optimal_cutoff(probabilities = test_over$incomeProbability, actuals = test_over$Prediction) ] = 1
