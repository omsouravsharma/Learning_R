# Importing Daily Stock Price Data. 

# Import Data
data = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/TCS.NS.csv", header = TRUE)
View(data) # View Data

# Convert the date variable from Character to a Date
class(data$Date) 

data$Date <-as.Date(data$Date, format = "%Y-%m-%d")

head(data)

class(data$Date)

Pg -23

plot(data$Close, x = data$Date)
