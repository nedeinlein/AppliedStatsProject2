# Remove outliers from single column
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

## USAGE
remove_outliers(df$age)


# Remove outliers from entire dataset
remove_outliers_from_df <- function(df){
  cols <- c(colnames(df))
  df[cols] <- sapply(df[cols], remove_outliers)
  return(df)
}

## USAGE
z <- df[c('age', 'education.num')]
z <- remove_outliers_from_df(z)


## This will help us see which columns are good candidates for imputation
library(reshape2)
make_plots <- function(df, plottype = "box", histo_var = NULL){
  if (plottype == "box"){
    melted_df <- melt(df)
    ggplot(data = melted_df, aes(x=variable, y = value)) +
      geom_boxplot(color='blue')
  }
  else {
    ggplot(data = df, aes(x=histo_var)) + geom_histogram() + xlab("X-variable")
  }
}


## Running the boxplots of each variable to check variance
make_plots(df = na.omit(z), 
           plottype = "box")
