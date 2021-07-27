library(cluster)    # clustering algorithms
library(factoextra) #  algos and viz
library(klaR) # Kmodes analysis

# Scale the data and start with a # of clusters
clustering_df <- scale(df2[, 2:7])
optimal_centers <- 6

# Take results of PCA analysis and enter into this
km_scaled <- kmeans(clustering_df, centers = optimal_centers)
km_scaled$cluster

# Get the centers for plotting
km_centers <- data.frame(cluster = factor(1:optimal_centers), km_scaled$centers)

#Visualize the clusters
fviz_cluster(km_scaled, data = clustering_df)

# Calculating ideal # of clusters using elbow method
wss <- function(k){
   kmeans(clustering_df, k, nstart = 10)$tot.withinss
}

k.values <- 3:9
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,
     type = "b", pch = 19, frame =FALSE,
     xlab = "Num Clusters of K",
     ylab = "Total WSS")
