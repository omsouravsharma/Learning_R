# Data frames and data types

# Data Types
# 1. Numeric 
# 2. Logical 
# 3. Character 
# 4. integer
# 5. complex
# 6. Raw

x <- 6
y <- "Scotland"
z <- TRUE

class(x)
class(y)
class(z)

number <-c(6,9,12)
class(number) # Class return the data types of the element of the vector. 

all <- c(x,y,z)
class(all) # R force the object in the vector of the same type

# Type of tables

# Dataframe- Columns(Variables) and Rows (observation)
# Matrix - same type data
# data.frame - most common
# data.table - enhanced version
# tbl_df - another enhances version 

?WorldPhones

class(WorldPhones)
View(WorldPhones)


?airquality
class(airquality)
View(airquality)


?msleep
class(msleep)
View(msleep)

# Convert data structure to another 
WorldPhones_DF <- as.data.frame(WorldPhones)
WorldPhones_DF
class(WorldPhones_DF)

airquality_matrix <- as.matrix(airquality)
airquality_matrix
class(airquality_matrix)


a <- "A raniy data in london"
class(a)

a <- 'A raniy data in london'
class(a)

b <- sum( 1+ 2, 1L + 2, 1L + 2L)
b
class(b)

"Hello" + 1

FALSE * 2
TRUE + 1

ncol(msleep)
nrow(msleep)
dim(msleep)
names(msleep)
row.names(msleep)


x <- 1:6
x

matrix(x, nrow = 2, ncol = 3)
matrix(x, nrow = 3, ncol = 2)

# ----------------------------------#
# Vectors in data frames

?airquality

data("airquality")

# Extract the temp vector
airquality$Temp

mean(airquality$Temp)

names(airquality)

#[i, j]
airquality[5,4]

airquality$Temp[5]

# Row
airquality[5,]

# Column
airquality[, 4]

airquality[[4]]

airquality[, c(2,4,6)]

airquality[, c("Temp", "Wind")]

airquality[, 3]

airquality[c(1:5), ]

cor(airquality[, 4], airquality[, 3])

airquality[, -c(3,4)]

# Using $ sign to update value

data = airquality

head(data)
data$Solar.R[6]
data$Solar.R[6] <- 222

# Using Condition 

max(airquality$Temp)

# To find the hottest day 
which.max(airquality$Temp) # 120

airquality[120,]

# OR
airquality[which.max(airquality$Temp), ]

# Lowest temperature
airquality[which.min(airquality$Temp), ]

# All data where temperature is 90 
airquality[airquality$Temp > 90, ]


a <- 3; b <- 8
a ==b
a>b
a<b
a!=b
is.na(a)
a %in% c(1,4,9)
b %in% c(1,4,8)


# To get indices of the element 
which(airquality$Temp > 90)

# all
all(airquality$Temp > 90) # FALSE because all element are not above 90 

#any
any(airquality$Temp > 90)

# Count 
sum(airquality$Temp > 90) # FALSE = 0, TRUE = 1 # Logical Values returned 

airquality$Temp > 90

# To get Proportion
mean(airquality$Temp > 90)

# AND &  OR  | 

a<b & a> 1
a<b & a ==4
a<b | a ==4
a==4 | a==5
xor(a<b, a==4)


# Coldest day 
airquality[which.min(airquality$Temp),]

sum(airquality$Wind > 17)

airquality[airquality$Wind > 17,]

sum(is.na(airquality$Ozone))

sum(airquality$Temp < 70 & airquality$Wind > 10)

?cut # Convert Numerical into categorical 



# Importing data

# Importing CSV files

getwd() # Show Current working directory

data <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/philosophers.csv")
data
View(data)

str(data)

# Importing using file.choose
data1<- read.csv(file.choose())
data1

# Import using RStudio

# Note: Please make sure file path is correct. 

# Importing Excel file 

install.packages("openxlsx")
library(openxlsx)
read.xlsx()

# SAVING AND EXPORTING YOUR DATA


age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)
bookstore <- data.frame(age, purchase)

write.csv(bookstore, "bookstore.csv")
write.xlsx(bookstore, "bookstore.xlsx")

# Save .RData format

save(bookstore, age, file = "myData.RData")
save.image(file = "allMyData.RData")


load(file = "myData.RData")

# Creating new project 

# Running t-test - used to test whether the mean of two population are equal. 


library(ggplot2)

carnivores <- msleep[msleep$vore == 'carni', ]
herbivores <- msleep[msleep$vore == 'herbi', ]

t.test(carnivores$sleep_total, herbivores$sleep_total)


t.test(carnivores$sleep_total, herbivores$sleep_total, conf.level = 0.90, 
       alternative = "greater", var.equal = TRUE)


# Fitting a linear Regression model 
?mtcars

View(mtcars)

# Relationship between gross horsepower and fuel consumption 
ggplot(mtcars, aes(hp, mpg)) + 
  geom_point()


m <- lm(mpg ~ hp, data = mtcars)
m
summary(m)

coef(m)

# Add regression line to plot: 
ggplot(mtcars, aes(hp, mpg)) + 
  geom_point()+ 
  geom_abline(aes(intercept = coef(m)[1], slope = coef(m)[2]), color = "red")

plot(m)

# Add more variable to the model 

m2 <- lm(mpg~ hp + wt, data= mtcars)
summary(m2)

plot(m2)



# Grouped summaries: 

aggregate(Temp ~ Month, data = airquality, FUN = mean)

# Aggregate removes the NA before computing mean 

# SD 
aggregate(cbind(Temp, Wind)~ Month, data = airquality, FUN = sd)

# Count 
aggregate(Temp ~ Month, data = airquality, FUN = length)

#by
by(airquality$Temp, airquality$Month, mean)


# Using %>% pipes

# Help code readability and restructure 

install.packages("magrittr")

airquality2 <- airquality[airquality$Temp > 80, ]

# Convert wind speed to m/s
airquality2$Wind <- airquality2$Wind * 0.44704

hot_wind_means <- aggregate(Wind ~ Month, data = airquality2, FUN = mean)
hot_wind_means


hot_wind_means <- aggregate(Wind*0.44704 ~ Month, data = airquality[airquality$Temp > 80, ], FUN = mean)
hot_wind_means

# magrittr introduces %>%, called pipe for chain function together. 
# data %>% function_1 %>% function_2 -> new_variable 

# Extract hot day ; Convert to m/s ; compute grouped mean

subset(airquality, Temp > 80)

library(magrittr)

# pipeline 
airquality %>%
subset(Temp > 80) %>%
inset("Wind", value = .$Wind*0.44704)%>%
aggregate(Wind ~ Month, data= ., FUN= mean) -> hot_wind_means

hot_wind_means

# read %>% as then; Read left to right 

#--------------------------------#
