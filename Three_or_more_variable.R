data("USJudgeRatings")
# Multiple regression

reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG+ DECI + PREP + FAMI+ ORAL + WRIT + PHYS, 
           data = USJudgeRatings)


reg1

summary(reg1)
resid(reg1)
hist(residuals(reg1))

# Backward
regb <- step(reg1, directions = "backward", trace = 0)
summary(regb)

# Forward

reg0 <- lm(RTEN ~ 1, data = USJudgeRatings)
reg0

  
regf<-  step(reg0, directions = "forward", 
             scope = (~CONT + INTG + DMNR + DILG + CFMG+ DECI + PREP + FAMI+ ORAL + WRIT + PHYS),
             
             data = USJudgeRatings,
             
             trace = 0)
summary(regb)


# 2 factor ANOVA

data("warpbreaks")
boxplot(breaks~ wool*tension, data = warpbreaks)

aov1 <- aov(breaks ~ 
              wool + tension + wool:tension, 
            data = warpbreaks
            )
aov1
summary(aov1)


model.tables(aov1)
model.tables(aov1, type = "means")
model.tables(aov1, type = "effects")


TukeyHSD(aov1)
boxplot()

# Cluster Analysis: 

data("mtcars")

mtcars1 <- mtcars[, c(1:4, 6:7, 9:11)]
mtcars[1:5,]

d <- dist(mtcars1)
d

c <- hclust(d)
c

plot(c)

g3 <- cutree(c, k = 3)
g3

gm <- cutree(c, k = 2:5)
gm

rect.hclust(c, k=2, border = "grey")
rect.hclust(c, k=3, border = "blue")


km <- kmeans(mtcars1, 3)
km

require(cluster)

clusplot(mtcars1, 
         km$cluster, 
         color = T, 
         shade = T, 
         lines = 3, labels = 2)

# PCA Principal Components/factor analysis



pc <- prcomp(mtcars1, 
             center = T, scale. = T)
pc

summary(pc)

plot(pc) # screeplot

pc

predict(pc)

biplot(pc)

factanal(mtcars1, 1)
factanal(mtcars1, 2)
factanal(mtcars1, 3)
factanal(mtcars1, 4)

data = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/StateClusterData.csv", header = T)
data
rownames(data) <- data[,1]
data[,1] <- NULL

d <- dist(data)
c <- hclust(d)
plot(c)

# Draw boxed 

rect.hclust(c, k = 2, border = "gray")
rect.hclust(c, k = 3, border = "blue")
rect.hclust(c, k = 4, border = "green4")
rect.hclust(c, k = 5, border = "darkred")
