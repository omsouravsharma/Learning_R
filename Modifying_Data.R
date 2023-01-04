# Outliers

OS <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/OS.CSV")
OS

# Combine into others 

OS.hi <- subset(OS, Proportion > 0.1)
OS.hi

 rivers
hist(rivers) 

boxplot(rivers, horizontal = TRUE)

rivers.low <- rivers[rivers < 1210]
boxplot(rivers.low, horizontal = TRUE)

# 2. Transforming Variables 

?islands
islands

hist(islands, breaks = 16)

boxplot(islands)

# z- scores 

islands.z <- scale(islands)
islands.z

hist(islands.z)

round(mean(islands.z),2)
sd(rivers.low)

attr(islands.z, "scaled:center")
attr(islands.z, "scaled:scale")


# Logrithmic Transformation

island.ln = log(islands)
island.ln

hist(island.ln)
boxplot(island.ln)

# Squaring 

# Ranking 

island.rank1 = rank(islands)
hist(island.rank1)

island.rank2 = rank(islands, ties.method = "random")
hist(island.rank2)


# Dichotomizing 
# Use Wiserly 

continent <- ifelse(islands > 1000, 1, 0)
continent
hist(continent)
plot(continent)


# Composite Variables 

rn1 <- rnorm(1000000)
rn1
hist(rn1)
summary(rn1)

rn2 <- rnorm(1000000)
rn2

# Vector based language 

rn.mean = (rn1 + rn2)/2
hist(rn.mean)

rn.prod = (rn1 * rn2)
hist(rn.prod)


# Kurtosis comparison
kurtosi(rn1)

library("psych")
kurtosi(rn2)


# Missing Values 

x1 <- c(1,2,3,NA, 5)
x1
summary(x1)
mean(x1)

which(is.na(x1))


mean(x1, na.rm = T)

x2 <- x1
x2[is.na(x2)] <-0
x2

x3 <- ifelse(is.na(x1), 0, x1)
x3

# Imputation 

# MICE multivariate 


skew <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/xskew.CSV")
skew 
x <- skew[,2]
str(x)

hist(x)
boxplot(x, horizontal = TRUE)

# Square the data

x2 <- x^2
hist(x2)
boxplot(x2)

# 4th Power 

x4 <- x2^2
hist(x4)
boxplot(x4)


