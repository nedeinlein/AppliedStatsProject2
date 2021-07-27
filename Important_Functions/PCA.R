# PCA  function
run_pca <- function(df, scale_data = TRUE){
  if(scale_data) {
    # Scale
    df = as.data.frame(scale(df))
  }
    # Run the PCA
    pca <- princomp(df)
    screeplot(pca)
    return(pca$loadings)
  }

run_pca(df2[c('age','fnlwgt', 'education.num', 'capital.gain', 'capital.loss', 'hours.per.week')], 
        scale_data = TRUE)
