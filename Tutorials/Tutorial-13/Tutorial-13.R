# Cluster Analysis. 

# 1. Hierarchical Clustering 
library(cluster)

?votes.repub
View(votes.repub)

clsuter_agnes <- agnes(votes.repub)

plot(clsuter_agnes, which = 2) #called dendogram

# agnes function provide two metric Euclidean (default) and Manhattan distance and different
# measuring methods average(default), single, complete, ward

# Use scale to standardized your data

cluster_agnes <- agnes(scale(votes.repub))
plot(cluster_agnes)


# Re-write with pipeline 

library(magrittr)
library(dplyr)
votes.repub %>% scale()%>%
  agnes()%>%
  plot(which = 2)

# Change metric and clustering method
votes.repub %>% scale() %>%
  agnes(metric = "manhattan", method = "complete") %>%
  plot(which = 2)

votes.repub %>% scale() %>%
  agnes(metric = "manhattan", method = "complete") %>%
  plot(which = 2, hang = -1)

# hang - make all obervation at same level

# Alternative to agnes is diana

votes.repub %>% 
  scale() %>%
  diana() %>%
  plot(which = 2)

install.packages("factoextra")
install.packages("dendextend")
library(factoextra)
library(ggplot2)
library(dendextend)

votes.repub %>% scale() %>%
  agnes()-> cluster_agnes

votes.repub %>% 
  scale() %>%
  diana()-> cluster_diana

tanglegram(as.dendrogram(cluster_agnes), as.dendrogram(cluster_diana))

# Cutting cluster 

votes.repub %>%
  scale() %>%
  hcut(k = 5, hc_fun = "agnes" ,
  hc_method = "average" ,
  hc_metric = "euclidean") %>%
  fviz_dend()

# Manhattan distance 

votes.repub %>%
  scale() %>%
  hcut(k = 5, hc_fun = "agnes" ,
       hc_method = "average" ,
       hc_metric = "manhattan") %>%
  fviz_dend()


# HEATMAPS and clustering variables

votes.repub %>%
  as.matrix() %>%
  heatmap()

# Scaling 
votes.repub %>%
  as.matrix() %>%
  heatmap(scale = "row")

votes.repub %>%
  as.matrix() %>%
  heatmap(scale = "col")

library(RColorBrewer)

col_palette <- colorRampPalette(brewer.pal(8, "BuPu"))(25)

votes.repub %>%
  as.matrix() %>%
  heatmap(scale = "row", col = col_palette)

# Centeroid - based clustering