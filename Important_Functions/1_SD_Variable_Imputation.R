## Imputation with sample taken from range of 1SD of the mean
## Defining the imputation function --> 
## generate values within 1SD of the variable mean to impute

imputation_1sd <- function(colname){
  ifelse(is.na(colname),
         round(sample((mean(colname, na.rm = TRUE) - 
                         sd(colname, na.rm = TRUE)):
                        (mean(colname, na.rm = TRUE) -
                           sd(colname, na.rm = TRUE)),
                      size = sum(is.na(colname)), replace = T),0), colname)
}
