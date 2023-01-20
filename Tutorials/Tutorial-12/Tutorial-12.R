# Principal component Analysis

path = "C:/Users/NEXT/Desktop/Learning_R/Data/seeds_dataset.txt"
seeds <- read.table(path, col.names = c("Area", "Perimeter", "Compactness", "Kernel_length", "kernel_width", 
                                        "Asymmetry", "Groove_length", "Variety"))

seeds$Variety <- factor(seeds$Variety)
?factor

library(ggplot2)
library(GGally)

ggpairs(seeds[, -8], aes(color= seeds$Variety, alpha=0.2))

# Compute Principal components:

pca <- prcomp(seeds[, -8], center = TRUE, scale=TRUE)
pca

summary(pca)

screeplot(pca, types="lines")

cor(pca$x[,1], pca$x[,2])
library(ggfortify)
autoplot(pca, data=seeds, color = "Variety")

autoplot(pca, data=seeds, color = "Variety", x = 2, y=3)
