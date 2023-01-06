# Calculating Correlation

data(swiss)
cor(swiss)
round(cor(swiss),2)

cor.test(swiss$Fertility, swiss$Education)

install.packages("Hmisc")
require("Hmisc")
rcorr(as.matrix(swiss))

rm(list = ls())

data("trees")
trees[1:5,]

hist(trees$Height)
hist(trees$Girth)
plot(trees$Height ~ trees$Girth)
abline(lm(trees$Height ~ trees$Girth))

# Linear Regression model

reg1 <- lm(Height ~ Girth,data = trees)
summary(reg1)


confint(reg1)

predict(reg1)
predict(reg1, interval = "prediction")



lm.influence(reg1)

# Comparing mean with t test
?sleep
sd <- sleep[, 1:2]
sd

hist(sd$extra, col = "lightgray")
boxplot(extra ~ group, data = sd)

t.test(extra ~ group, data = sd)


t.test(extra ~ group, data = sd, 
       alternative = "less", 
       conf.level = 0.08)

x <- rnorm(30, mean = 20, sd=5)
#x

y <- rnorm(30, mean = 22, sd=5)
#y

t.test(x,y)


# Comparing paired mean: Paired T test

t1 <- rnorm(50 ,mean= 52, sd = 6)
#t1

dif = rnorm(50, mean= 6, sd = 12)
#dif

t2 <-t1 + dif
t2
hist(t1)
hist(dif)
hist(t2)

boxplot(t1, t2)

require(MASS)

# Parallel Coordinate plot

pairs <- data.frame(t1,t2)
#pairs

parcoord(pairs, var.label = TRUE)

t.test(t2, t1, paired = TRUE)

t.test(t2,t1, paired = TRUE, 
       alternative = "greater",
       mu = 6,
       conf.level = 0.99)

rm(list = ls())


# Comparing means with ANOVA

x1 <- rnorm(30, mean = 40, sd = 8)
x2 <- rnorm(30, mean = 41, sd = 8)
x3 <- rnorm(30, mean = 45, sd = 8)
x4 <- rnorm(30, mean = 45, sd = 8)

boxplot(x1, x2, x3, x4)

xdf <- data.frame(cbind(x1,x2,x3,x4))
xdf

summary(xdf)

xs <-stack(xdf)
View(xs)


anova1 <- aov(values ~ ind, data = xs)
anova1


summary(anova1)


TukeyHSD(anova1)


# Comparing Proportions

n5 <- c(rep(100, 5))
x5 <- c(65,60,60,50,45)

prop.test(x5, n5)


# CI .95

n2 <- c(40,40)
x2 <- c(30,20)

prop.test(x2, n2, conf.level = 0.80)


# Cross tab categorical variables 

?Titanic

Titanic

ftable(Titanic)

# Convert table to d ate frame with one row per observations
tdf <- as.data.frame(lapply(as.data.frame.table(Titanic), 
function(x)rep(x, as.data.frame.table(Titanic)$Freq)))[,-5]

tdf[1:5,]


# Create Contingency table

ttab <-table(tdf$Class, tdf$Survived)
ttab

round(prop.table(ttab,1),2)*100

tchi <- chisq.test(ttab)
tchi

tchi$observed
tchi$expected
tchi$residuals
tchi$stdres


# Robust statics  - 

install.packages("quantreq")
require(quantreg)

?rq
data(engel)
attach(engel)


plot(income, 
     foodexp, 
     xlab = "Household Income", 
     ylab = "Food Expenditure", 
     type = "n", 
     cex = .5)
points(income, 
       foodexp, 
       pch = 16, 
       col = "lightgray")

taus <- c(.05, .1, .25, .75, .9, .95)
xx <- seq(min(income), max(income), 100)
f = coef(rq((foodexp)~(income), tau=taus))

yy <- cbind(1, xx)%*%f

for (i in 1:length(taus)) {
  lines (xx, yy[, i], col = "darkgray")
}

abline(lm(foodexp ~ income), 
       col = "darkred", 
       lwd = 2)


abline(rq(foodexp ~ income), 
       col = "blue", 
       lwd = 2)
  
legend(3000, 1000,
       c("mean fit", "median fit"), 
       col = c("darkred", "blue"), 
       lty = 1, 
       lwd = 2
       )


# Challenge - 

data <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/mlb2011.csv")
attach(data)

prop.test(data$HomeWins, data$AllWins)

x2 <- c(31, 57)
n2 <- c(72, 96)

prop.test(x2, n2)



