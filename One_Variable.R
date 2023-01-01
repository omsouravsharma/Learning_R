# Calculating frequencies

groups <- c(rep("blue", 3990),
           rep("red", 4140),
           rep("orange", 1890),
           rep("green", 3770),
          rep("purple", 855)
           )

# Create Frequency Tables

group.t1 <- table(groups)
group.t1

group.t2 <- sort(group.t1, decreasing = TRUE)
group.t2

prop.table(group.t2)
group.t3 <- round(prop.table(group.t2),2)*100

barplot(group.t3, 
        horiz = TRUE,
        col = brewer.pal(5, "Set2"))

?RColorBrewer

# Calculating Descriptive. 

?cars
cars
str(cars)

# Calculate Description

summary(cars$speed)
summary(cars)
boxplot(cars$speed)

fivenum(cars$speed)

boxplot.stats(cars$speed)

help(package = 'psych')
install.packages('psych')

require('psych')

describe(cars)

# Inferential Statistics 
# Single Proportion: Hypothesis test and confidence interval

prop.test(98,162)

prop.test(98,162, alt = "greater", conf.level = .90)


?quakes

quakes[1:5,]
mag <- quakes$mag
mag
mag[1:5]

t.test(mag)

t.test(mag, alternative = "greater", mu = 4)


# Goodness of fit test. 
?HairEyeColor
str(HairEyeColor)
eyes <- margin.table(HairEyeColor, 2)
eyes

round(prop.table(eyes),2)


# Pearson's chi-square test


chi1 <- chisq.test(eyes)
chi1

chi2 <- chisq.test(eyes, p=c(.41,.32,.15,.12))
chi2


# Robust Statistics for uni variate analysis

?state.area
data(state.area)
area <- state.area
area
hist(area)

boxplot(area)
boxplot.stats(area)

mean(area)
median(area)
mean((area), trim = 0.05)


sd(area)
mad(area)


IQR(area)
fivenum(area)

mtcars


str(mtcars)


mean(mtcars$mpg)

d <-describe(mtcars[c(1,4,7)])
d

d[, c(3,4,11,12)]
