# Centeroid - based clustering
library(cluster)

seeds <- read.table("C:/Users/NEXT/Desktop/Learning_R/Data/seeds_dataset.txt", 
                    col.names = c("Area", "Perimeter", "Compactness", "Kernel_length", "Kernel_width", "Asymmetry", "Groove_lenght", "Variety"))
seeds$Variety <- factor(seeds$Variety)

# If you are not aware of data then use mean- clustering  - based on distance to the cluster mean - centriods

library(magrittr)
library(dplyr)
seeds[, -8] %>%
  scale() %>%
  kmeans(centers = 3) -> seeds_cluster

seeds_cluster

pca <- prcomp(seeds[, -8])
library(ggfortify)


autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)

#change to 2 
seeds[, -8] %>%
  scale() %>%
  kmeans(centers = 2) -> seeds_cluster

seeds_cluster

autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)


#change to 4
seeds[, -8] %>%
  scale() %>%
  kmeans(centers = 4) -> seeds_cluster


autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)

#change to 8
seeds[, -8] %>%
  scale() %>%
  kmeans(centers = 8) -> seeds_cluster


autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)

#change to 12
seeds[, -8] %>%
  scale() %>%
  kmeans(centers = 12) -> seeds_cluster


autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)

#pam

seeds[, -8] %>%
  scale() %>%
  pam(k=3) -> seeds_cluster


autoplot(pca, data = seeds, colour = seeds_cluster$cluster, 
         shape = "Variety", 
         size = 2, 
         alpha = 0.75)

install.packages("factoextra")
library(factoextra)

#wss
fviz_nbclust(scale(seeds[, -8]), pam, method = "wss")

#silhouette
fviz_nbclust(scale(seeds[, -8]), pam, method = "silhouette")

#gap_stat
fviz_nbclust(scale(seeds[, -8]), pam, method = "gap_stat")

#first, find the cluster
seeds[, -8]%>%
  scale()%>%
  kmeans(centers = 3) -> seeds_cluster


# Plot cluster and their convex hulls 
library(factoextra)

fviz_cluster(seeds_cluster, data = seeds[, -8])

# without row numbers
fviz_cluster(seeds_cluster, data = seeds[, -8], geom = "point")

# with ellipses on the multivariate normal distribution 

fviz_cluster(seeds_cluster, data = seeds[, -8], geom = "point", ellipse.type = "norm")


# Fuzzy clustering 

seeds[, -8]%>%
  scale()%>%
  fanny(k = 3) -> seeds_cluster

#check membership 
seeds_cluster$membership


# plot closet hard clustering 

fviz_cluster(seeds_cluster, geom = "point")

# How many cluster to use. 
seeds[, -8]%>%
  scale()%>%
  fviz_nbclust(fanny, method = "wss")

seeds[, -8]%>%
  scale()%>%
  fviz_nbclust(fanny, method = "silhouette")

# seeds[, -8]%>%
#   scale()%>%
#   fviz_nbclust(fanny, method = "gap")

# Model Based clustering 
#mclust
install.packages("mclust")

library(mclust)

seeds_cluster <- Mclust(scale(seeds[, -8]))
summary(seeds_cluster)

fviz_cluster(seeds_cluster, geom = "point", ellipse.type = "norm")


# Comparing clusters

seeds[, -8]%>%
  scale()%>%
  kmeans(centers = 3)-> seeds_cluster

# Add result to data frame
seeds$cluster <- factor(seeds_cluster$cluster)

# Instead of cluster we use $clustering for agnes, pam and fanny

# object and $classification for an Mclust object

# Compare the area of the 3 clusters using boxplot

library(ggplot2)

ggplot(seeds, aes(x = Area, group = cluster, fill = cluster))+
  geom_boxplot()

ggplot(seeds, aes(x = Area, group = cluster, fill = cluster))+
  geom_density(alpha = 0.7)

library(GGally)
ggpairs(seeds[, -8], aes(colour = seeds$cluster, alpha = 0.2))
