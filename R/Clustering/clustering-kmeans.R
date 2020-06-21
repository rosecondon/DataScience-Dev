
# Attribute(predictors) Information:
# sepal length(in cm)
# sepal width(in cm)
# petal length(in cm)
# petal width(in cm)
# class: – Iris Setosa – Iris Versicolour – Iris Virginica

# Use the R function kmeans to cluster the points as well as possible. 
# Report the best combination of predictors, your suggested value of k, and how well your best clustering predicts flower type

rm(list = ls())
library(datasets)
d.iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header=FALSE, col.names=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"))
head(d.iris)

data(iris)
head(iris)
summary(iris)
# Required R packages and functions
# kmeans(x, centers, iter.max = 10, nstart = 1)
# load libraries
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# Euclidean distance || Manhattan distance
sample_iris = sample(nrow(iris), size = floor(nrow(iris) * 0.3))
distance <- get_dist(sample_iris)
fviz_dist(distance, gradient = list(low = "red", mid = "white", high = "blue"))
# K-Means Clustering => standard algorithm is the Hartigan-Wong algorithm
# 1. Specify the number of clusters (K)
iris.scaled <- scale(iris[, -5])
k2 <- kmeans(iris.scaled, centers=3, nstart = 25)
str(k2)
# view group results
k2
# View cluster with
# fviz_cluster(k2, iris[, -5], ellipse.type = "norm")
fviz_cluster(k2, iris[, -5], palette = "Set2", ggtheme = theme_minimal()) # Cluster plot with different theme
# 2. Select randomly k objects from the data set as the initial cluster centers or mean
# 3. Assigns each observation to their closest centroid
# 4. For each of the k clusters update the cluster centroid by calculating mean
k3 <- kmeans(iris.scaled, centers = 4, nstart = 25)
k4 <- kmeans(iris.scaled, centers = 5, nstart = 25)
k5 <- kmeans(iris.scaled, centers = 6, nstart = 25)
# compare plots
p1 <- fviz_cluster(k2, geom = "point", data = iris.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = iris.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = iris.scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = iris.scaled) + ggtitle("k = 5")

library(gridExtra)
# grid.arrange(p1, p2, nrow = 2)
# grid.arrange(p3, p4, nrow = 2) # group 2 to avoid reached elapsed time limit

# 5. Iteratively minimize the total within sum of square
# Elbow Method
set.seed(123)
# function to compute total within-cluster sum of square 
sumOfSquare <- function(k) {
  kmeans(iris.scaled, k, nstart = 10)$tot.withinss
}
# Compute and plot sum of square for k = 1 to k = 15
k.values <- 1:15
# extract sum of square for 2-15 clusters
sumOfSquare_values <- map_dbl(k.values, sumOfSquare)
# View with plot
plot(k.values, sumOfSquare_values,
     type="b", pch = 19, frame = FALSE,
     xlab="Clusters Number K",
     ylab="Total sum of squares within Cluster K")
# or view with fviz_ method
fviz_nbclust(iris.scaled, kmeans, method = "sumOfSquare")

# Step 6: select suggested value of k
k_best <- 4
set.seed(123)
final <- kmeans(iris.scaled, k_best, nstart = 25)
print(final)

# view with fviz_cluster
fviz_cluster(final, data = iris.scaled, ellipse.type = "norm")