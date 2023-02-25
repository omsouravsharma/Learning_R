# Importing Daily Stock Price Data. 

# Import Data
data = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/TCS.NS.csv", header = TRUE)
View(data) # View Data

# Convert the date variable from Character to a Date
class(data$Date) 

data$Date <-as.Date(data$Date)

head(data)

class(data$Date)


Pg -23
plot(data$Close)


# Sort Data 
data = data[order(data$Date), ]
head(data)

# convert data.frame object to xts which is xtensible time series 
class(data)

library("xts")

data <- as.xts(data[, 2:7], order.by = data$Date)
mode(data) <- "numeric"
head(data)

class(data)




plot(data$Close)


# Loading data directly from Yahoo finance

# library(quantmod)
# data.tcs <- getSymbols("TCS.NS", from = "2010-12-31", to = "2013-12-31", auto.assign = FALSE)
# data.tcs

#class(data.tcs)


summary(data)

# Checking Dimension
dim(data)

#First row
data[1,]

# First and last row

data[c(1, nrow(data)),]

# Keeping close column 

data[, 4]

data[1, c(4:5)]

# Sub-setting using dates

class(data)
TCS2012<- subset(data[,4], 
  index(data)>="2012-01-01" & index(data)<="2012-12-31")
TCS2012

# Convert to Dataframe

TCS2012 <- cbind(index(data), data.frame(data[,4]))
TCS2012[c(1:3, nrow(TCS2012)), ]

PG 39
