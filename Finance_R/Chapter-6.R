# Individual Security Returns

# Price Return
# Price Retun = (P/P(Y)) -1 Where P is price today, P(Y) = Price yesterday
#data
library(xts)
library(quantmod)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)
head(data.INFY)

# Closing price

INFY.PRC.RET <- data.INFY[,4]
head(INFY.PRC.RET)


# Calculate the Price Return 

INFY.PRC.RET$INFY.PRC.RET <- Delt(INFY.PRC.RET$Close)

head(INFY.PRC.RET)

