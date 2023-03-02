# Volatility - Bollinger Bands 

# Bollinger Band
# 1. 20 day SMA
# 2. Upper Band 2 standard Deviation above 20 SMA 
# 3. Lower Band 2 standard Deviation below 20 SMA

library(xts)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)


head(data.INFY)

INFY.BB <-  data.INFY[,4]
INFY.BB[c(1:3, nrow(INFY.SMA)), ]


# Calculate the 20 Day Mean and standard Deviation

INFY.BB$avg <- rollmeanr(INFY.BB$Close, k = 20)
INFY.BB$sd <- rollapply(INFY.BB$Close, width = 20, FUN = sd, fill = NA)
INFY.BB[c(1:3, nrow(INFY.BB)),]

# Calculate the Bollinger Band

INFY.BB$sdup <- INFY.BB$avg + 2*INFY.BB$sd
INFY.BB$sddown <- INFY.BB$avg - 2*INFY.BB$sd

INFY.BB[c(1:3, nrow(INFY.SMA)), ]


# Plot the Bollinger Band

plot(x = index(INFY.BB),
     xlab = "Date",
     y = INFY.BB$Close,
     ylim = range(INFY.BB[,-3], na.rm = TRUE),
     ylab = "Price INR",
     type = 'l',
     lwd = 3,
     main = "Bollinger Band (20Day, 2 SD)")

lines(x = index(INFY.BB), y = INFY.BB$avg, lty = 2, col = "red")
lines(x = index(INFY.BB), y = INFY.BB$sdup,  col = "gray40")
lines(x = index(INFY.BB), y = INFY.BB$sddown,  col = "gray40")
legend("topleft", 
       c("INFY Price","20-Day Moving Average","Upper Band","Lower Band"),
      lty=c(1,2,1,1),
      lwd=c(3,1,1,1),
      col=c("black","black","gray40","gray40"))
