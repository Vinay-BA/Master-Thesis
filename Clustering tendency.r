library(cluster)
library(factoextra)
library(magrittr)
library(xlsx)
library(NbClust)
library(dbscan)
library(tsne)
library(ggplot2)

# Import data file
alcohol_act <- read.xlsx(file = "clustering_data.xlsx",sheetIndex = 1)
#alcohol_act <- alcohol_act[c(3,4,6,7)] 
# Remove rows with 0 for all columns
#alcohol_act <- alcohol_act[rowSums(alcohol_act[, -1])>0, ]

# Select required columns
# alcohol_act <- alcohol[c(4,7,8,9,10,11,12)]

# Remove rows with NA's for all variables
# my_data <- my_data[rowSums(is.na(my_data)) != ncol(my_data), ]

# Replace NA's with 0's
# my_data[is.na(my_data)] = 0

# Log transformation
alcohol_log <- log(alcohol_act+1)

# Square-root transformation
alcohol_sqrt <- sqrt(alcohol_act)

## Scale
#my_data_scale <- scale(my_data)
#nrow(my_data_scale)

# Clustering tendency
#gradient.color <- list(low = "steelblue",  high = "white")
#my_data_scale %>% get_clust_tendency(gradient = gradient.color, n = 4000)
#my_data %>% get_clust_tendency(gradient = gradient.color, n = 4000)

#res <- get_clust_tendency(my_data_scale, n = nrow(my_data_scale)-1, graph = FALSE)
#res$hopkins_stat

#### 7 features ####
act_7features <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "kmeans")

log_7features <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "kmeans")

sqrt_7features <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "kmeans")

act_7features_ward <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D")

log_7features_ward <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "ward.D")

sqrt_7features_ward <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "ward.D")

#### 4 features ####
act_4features <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "kmeans")

log_4features <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "kmeans")

sqrt_4features <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "kmeans")

act_4features_ward <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D")

log_4features_ward <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "ward.D")

sqrt_4features_ward <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "ward.D")

#### 7 features, canberra distance ####
act_7features_canb <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

log_7features_canb <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

sqrt_7features_canb <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

act_7features_ward_canb <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

log_7features_ward_canb <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

sqrt_7features_ward_canb <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

act_7features_wardD2_canb <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D2", distance = "canberra")

#### 4 features, canberra distance ####
act_4features_canb <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

log_4features_canb <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

sqrt_4features_canb <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "kmeans", distance = "canberra")

act_4features_ward_canb <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

log_4features_ward_canb <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

sqrt_4features_ward_canb <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "ward.D", distance = "canberra")

#### 7 features, euclidean, alllong ####
act_7features <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "kmeans", index = "alllong")

#log_7features <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "kmeans", index = "alllong")

#sqrt_7features <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "kmeans", index = "alllong")

act_7features_ward <- NbClust(data = alcohol_act, min.nc = 2, max.nc = 30, method = "ward.D", index = "alllong")

#log_7features_ward <- NbClust(data = alcohol_log, min.nc = 2, max.nc = 30, method = "ward.D", index = "alllong")

#sqrt_7features_ward <- NbClust(data = alcohol_sqrt, min.nc = 2, max.nc = 30, method = "ward.D", index = "alllong")


# Elbow method, PAM
fviz_nbclust(x = alcohol_act, FUNcluster = cluster::pam, method = "wss",
             diss = dist(alcohol_act, method = "canberra"), k.max = 30)+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
#3 clusters, euclidean distance
#11 clusters, canberra distance

# Silhouette method, PAM
fviz_nbclust(x = alcohol_act, FUNcluster = cluster::pam, method = "silhouette",
             diss = dist(alcohol_act, method = "canberra"), k.max = 30)+
  labs(subtitle = "Silhouette method")
#2,3 clusters, euclidean distance
#3,4 clusters, canberra distance

# Gap statistic, PAM
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(x = alcohol_act, FUNcluster = cluster::pam,  method = "gap_stat", nboot = 50, 
             diss = dist(alcohol_act, method = "canberra"), k.max = 30)+
  labs(subtitle = "Gap statistic method")

# Hierarchical clustering, ward
dist_mat <- dist(alcohol_act, method = "canberra")
hclust_ward <- hclust(dist_mat, method = 'ward.D')
cut_ward <- cutree(hclust_ward, k = 2)
df_cut_ward <- as.data.frame(table(cut_ward))
p<-ggplot(data=df_cut_ward, aes(x=cut_ward, y=Freq)) + geom_bar(stat="identity")
p

# k distance graph, To find suitable value for 
data(iris)
iris <- as.matrix(iris[,1:4])
kNNdistplot(iris, k=4)

alcohol <- as.matrix(alcohol_act)
alcohol_dist <- dist(x = alcohol, method = "canberra")
kNNdistplot(alcohol_dist, k=14)

## Not run:
#colors = rainbow(length(unique(iris$Species)))
#names(colors) = unique(iris$Species)
ecb = function(x,y){ 
  plot(x,t='n'); 
  text(x) 
  }
tsne_alcohol = tsne(alcohol_act[,], epoch_callback = ecb, perplexity=50)
# compare to PCA
dev.new()
pca_iris = princomp(iris[,1:4])$scores[,1:2]
plot(pca_iris, t='n')
text(pca_iris, labels=iris$Species,col=colors[iris$Species])



