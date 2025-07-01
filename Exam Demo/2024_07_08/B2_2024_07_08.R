library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)

# We aim to investigate the similarities among various underground metro networks by analyzing their station
# connectivity and network structure. One of your collaborators has devised a metric distance measure for networks
# based on metro lines, station connections, and accessibility. The file metro.txt contains the matrix of the pairwise
# distances1 between the underground metro networks of 90 di↵erent cities. In order to explore the similarities
# between these networks, we will employ a cluster analysis approach.

metro <- read.table("2024_07_08/metro.txt", h=TRUE)

# a) Which clustering methods discussed in class are suitable for this case?

# As we work with dissimilarity matrix we can applyt these approaches:
# 1. DBSCAN
# 2. Hierarchical clustering with average linkage
# 3. Hierarchical clustering with single linkage
# 4. Hierarchical clustering with complete linkage

# Requires source variabels
# 1. K-means
# 2. Hierarchical clustering with Ward linkage

# b) Perform hierarchical clustering of the metro networks using the different linkages you have at your disposal.
# What is the number of clusters that should be discovered according to you? Which linkage(s) would you exclude?

m.dist <- as.dist(metro)
metro.hclust_single <- hclust(m.dist, method='single')
plot(metro.hclust_single) # I would say, that here may be 3 clusters
# but one of them "Fortaleza" have only one element, so I woul be exlude this method

metro.hclust_average <- hclust(m.dist, method='average')
plot(metro.hclust_average) # here may see 3 clusters, pretty clear

metro.hclust_complete <- hclust(m.dist, method='complete')
plot(metro.hclust_complete) # here may see 3 clusters, pretty clear

# overall: 3 clusters, exlude single

# c) Based on the cophenetic correlation coeffcient, identify the best linkage.
# As bigger this coefficinet - as better clusterisation 
coph.single <- cophenetic(metro.hclust_single)
single_cor <- cor(m.dist, coph.single)
single_cor # 0.5746898 ~ 0.575

coph.average <- cophenetic(metro.hclust_average)
average_cor <- cor(m.dist, coph.average)
average_cor # 0.7704496 ~ 0.770 have highest value, best one is average linkage

coph.complete <- cophenetic(metro.hclust_complete)
average_cor <- cor(m.dist, coph.complete)
average_cor # 0.7539529 ~ 0.754 

# d) Now, let us explore the DBSCAN method.
# Using a minPts value of 5 and choosing a consistent value for eps (with an accuracy of .05), run the DBSCAN
# algorithm. Is the result satisfactory?

minPts <- 5 # defince our core points
dbs <- dbscan(m.dist, eps = 0.05, minPts)
dbs

# DBSCAN clustering for 90 objects.
# Parameters: eps = 0.05, minPts = 5
# Using unknown distances and borderpoints = TRUE
# The clustering contains 6 cluster(s) and 46 noise points.
# 
# 0  1  2  3  4  5  6 
# 46 11  6  6  9  7  5 
# 
# Available fields: cluster, eps, minPts, metric, borderPoints
# So we have 46 non clustered points 

# Plot of the resulting clustering
plot(m.dist, col = dbs$cluster + 1L, pch=19)

# I can not say, that this params, best, because we have too many noisy (not clustered points)
# probably - increasing eps we will improve situation 

# e) Is there a way to visualize the underground metro networks in a two-dimensional plot? Report the plot, showing
# through it the results of the best clustering procedure identified. Assess whether the plot tends to underestimate
# or overestimate the true distances.

location <- cmdscale(m.dist, k=2) # first arguments dissimilariy matrix,
# second - N-dimension where we want to transform data
location

# perform clustering on after MDS
location.dist <- dist(location)
minPts <- 5 # defince our core points
dbs_loc <- dbscan(location.dist, eps = 0.25, minPts)
dbs_loc

plot(location, col = dbs_loc$cluster + 1L, pch=19) # I would say, that it best clustering

# I have to set asp=1 (equal scales on the two axes)
# to correctly represent Euclidean distances
plot(location[,1], location[,2], type='n', asp=1, axes=FALSE, main="MDS of Metro stations",xlab='',ylab='')
points(location, col = dbs_loc$cluster + 1L, pch=19)
text(location[,1], location[,2], labels=colnames(as.matrix(m.dist)), cex = 0.75, pos = 3)

# change the sign to get the North in the upper part of the plot
plot(location[,1], -location[,2], type='n', asp=1, axes=FALSE, main="MDS of European cities",xlab='',ylab='')
text(location[,1], -location[,2], labels=colnames(as.matrix(eurodist)), cex = 0.75, pos = 3)

# compare the original matrix d_ij = d(x_i,x_j) and delta_ij = d(y_i,y_j) 
plot(m.dist, dist(location)) # have to be close to line (new distance have to be proportional to source distance)
# If points lies above diagonal - overestimates (distance after MDS > true distance)
# If points lies under diagonal - underestimates (distance after MDS < true distance)
# In our case many points lies above line => overestimates
