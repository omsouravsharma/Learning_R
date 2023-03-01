# Technical Analysis
# Trend Indicator
# Volatile Indicator
# Momentum Indicator 

library(xts)
# Simple Moving Average Crossover (SMA)
# 50-200

#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)

# names(data.INFY) <- paste(c("INFY.Open", "INFY.High", "INFY.Low","INFY.Close", 
#                             "INFY.Adj.Close", "INFY.Volume"))


head(data.INFY)

INFY.SMA <-  data.INFY[,4]
INFY.SMA[c(1:3, nrow(INFY.SMA)), ]

# Calculating the Rolling 50 Day and 200 Day Average Price

INFY.SMA$sma50 <- rollmeanr(INFY.SMA$Close, k=50)
INFY.SMA$sma200 <- rollmeanr(INFY.SMA$Close, k=200)

INFY.SMA[c(1:3, nrow(INFY.SMA)), ]

plot(x = index(INFY.SMA), 
     xlab = "Date", 
     y = INFY.SMA$Close, 
     ylab = "Price (INR)", 
     ylim = range(INFY.SMA, na.rm = TRUE),
     type = 'l', 
     main = "INFY 50-200 SMA")
lines(x = index(INFY.SMA), y = INFY.SMA$sma50, col = "blue")
lines(x = index(INFY.SMA), y = INFY.SMA$sma200, lty = 2, col = "red")
legend("topleft", )


legend("topleft",
         c("INFY Price","50-Day Moving Average","200-Day Moving Average"),
          col = c("black", "blue", "red"),
         lty=c(1,1,2))

