# Bar Charts
InsectSprays

spray <- InsectSprays

mean <- aggregate(spray$count ~ spray$spray, FUN = mean)
mean
plot(mean)

mean.data <- t(mean[-1]) # Remove first column
plot(mean.data)

mean.data
colnames(mean.data) <- mean[,1]

barplot(mean.data, 
        col = 'lightblue', 
        main = "Effeciveness of Insect Spray", 
        xlab = "Spray Used", 
        ylab = "Insect Count")


# Grouped Boxplot 

require(MASS)
?painters

painters


boxplot(painters$Expression ~ painters$School)
require(RColorBrewer)
boxplot(painters$Expression ~ painters$School, 
        col = brewer.pal(8, "Pastel2"), 
        names = c("Renais", "Mannerist", "Seicento", "Venetian", "Lombard", "16th C", "17th C", "French"),
#        notch = TRUE,
boxwex = 0.5, 
whisklty = 1, 
sapletly = 0, 
outpch = 16,
outcol = brewer.pal(8, "Pastel2"), 
main = "Expression Rating of Painters by School", 
ylab = "Painters School", 
xlab = "Expression Ratings")


# Scatterplot 

cars
str(cars)

plot(cars, 
     pch = 16, 
     col = "grey", 
     main = "Speed vs stopping distance in Cars 1920 ", 
     xlab = "Speed (MPH)", 
     ylab = "Stopping Distance")


# Linear Regression line 
abline(lm(cars$dist ~ cars$speed), 
       col = "darkred", 
       lwd = 2)

lines(lowess(cars$speed, cars$dist), 
       col = "blue", 
       lwd = 2)

install.packages("car")
require(car)

scatterplot(cars$dist ~ cars$speed ,
     pch = 16, 
     col = "darkblue", 
     main = "Speed vs stopping distance in Cars 1920 ", 
     xlab = "Speed (MPH)", 
     ylab = "Stopping Distance")


searchdata = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/SearchData.csv")
searchdata
str(searchdata)

boxplot(searchdata$nfl ~ searchdata$region, 
        col = brewer.pal(4, "Set2"),
        boxwex = 0.5, 
        whisklty = 1,
        staplelty = 0, 
        outpch = 16, 
        outcol = brewer.pal(4, "Set2"),
        main = "Google searcg Interetst in NFL", 
        xlab = "Region of US",
        ylab = "Search Interest"
        )

searchdata[searchdata$region == 'Midwest', ]
searchdata$region
