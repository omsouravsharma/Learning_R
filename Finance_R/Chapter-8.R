# Weekly
library(quantmod)
library(xts)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)
head(data.INFY)

wk <- data.INFY

wk[c(1:3, nrow(wk)),]

# Convert the daily data to weekly

INFY.WEEKLY <- to.weekly(wk)
head(INFY.WEEKLY)

# Calculate Weekly Return
INFY.WEEKLY <- INFY.WEEKLY[,4]
INFY.WEEKLY$RET <- Delt(INFY.WEEKLY$wk.Close)
INFY.WEEKLY[c(1:3, nrow(INFY.WEEKLY)),]

# Monthly
mo <- data.INFY
#Convert to monthly
INFY.MONTHLY <- to.monthly(mo)
INFY.MONTHLY<- INFY.MONTHLY[,4]
INFY.MONTHLY$RET <- Delt(INFY.MONTHLY$mo.Close)
INFY.MONTHLY[c(1:3, nrow(INFY.MONTHLY)),]
INFY.MONTHLY
