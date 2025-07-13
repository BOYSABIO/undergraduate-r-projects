#install.packages('cluster')
#install.packages('factoextra')
library(cluster)
library(factoextra)

# RETRIEVE DATA
data = USArrests
print(summary(data))

# STANDARDIZATION (Center, Scale) using scale()
data_scale = scale(data) #Scale function to scale the data (standardize)
print(summary(data_scale)) #Means are moved to zero (Above 0 = high, Below 0 = low)

# KMEANS, KMEDIODS
# CHOOSING K
fviz_nbclust(data_scale, kmeans, method = "wss") #Use data and Kmeans, choose method (wss) within sum of squared
fviz_nbclust(data_scale, kmeans, method = "silhouette") #Also silhouette method

km = kmeans(data_scale, centers = 2, nstart = 25) #Center is number of clusters, nstart is number of times it will run
print("KM: ")
print(km)

kme = pam(data_scale, k = 2, nstart = 25)
print("KME: ")
print(kme)
 
# VIZ
print("KM VIZ: ")
print(fviz_cluster(km, data = data_scale, choose.vars = c("Murder", "UrbanPop")))

print("KME VIZ: ")
print(fviz_cluster(kme, data = data_scale, choose.vars = c("Murder", "UrbanPop")))

km = kmeans(data_scale, centers = 4, nstart = 25) #Four Clusters
print("KM FOUR CLUSTERS:")
print(km)

print("KM VIZ FOUR CLUSTERS")
print(fviz_cluster(km, data = data_scale, choose.vars = c("Murder", "UrbanPop")))

# HIERARCHICAL CLUSERING
# Bottom up approach
d = dist(data_scale, method = "euclidean")
hc1 = hclust(d, method = "complete")
plot(hc1)

# More plotting
sub_group = cutree(hc1, k = 4) #Cut the tree into four clusters

fviz_cluster(list(data = data_scale, cluster = sub_group), 
        choose.vars = c("Murder", "UrbanPop"))

fviz_nbclust(data_scale, hcut, method = "wss")
fviz_nbclust(data_scale, hcut, method = "silhouette")