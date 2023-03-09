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

#Logarithmic Total Return

# formula rt = ln(pt/Pt-1) = ln(1+ Rt) = ln Pt = ln Pt-1 ln = natural logrithum

# Calculate Log Returns

INFY.LOG.RET <- INFY.PRC.RET$Close

INFY.LOG.RET$INFY.LOG.RET <- diff(log(INFY.LOG.RET$Close))
head(INFY.LOG.RET)

# combine both return

tot.rets<- cbind(INFY.PRC.RET[,2], INFY.LOG.RET[,2])
head(tot.rets)

max(abs(tot.rets$INFY.PRC.RET - tot.rets$INFY.LOG.RET), na.rm = TRUE)
min(abs(tot.rets$INFY.PRC.RET - tot.rets$INFY.LOG.RET), na.rm = TRUE)

# cumulative multi-day return
INFY.ACM <- INFY.PRC.RET[,2]
head(INFY.ACM)

# Set first day to 0 
INFY.ACM[1,1] <- 0
head(INFY.ACM)

# Calculate the Gross daily return

INFY.ACM$GROSS.RET <- 1+INFY.ACM$INFY.PRC.RET
head(INFY.ACM)

#Calculate the cumulative Gross Return

INFY.ACM$GROSS.CUM <- cumprod(INFY.ACM$GROSS.RET)
head(INFY.ACM)

#Convert Cumulative Return to Cumulative Net Return

INFY.ACM$NET.CUM <- INFY.ACM$GROSS.CUM-1
INFY.ACM[c(1:3, nrow(INFY.ACM)),]

# Calculating Logarithmic Return

#r1 to T = ln((1+R1)X(1+R2)..X(1+Rt))


INFY.LOGCUM <- INFY.LOG.RET
INFY.LOGCUM[c(1:3, nrow(INFY.LOGCUM)),]
View(INFY.LOGCUM)
# Set first return to 0 

INFY.LOGCUM[1,1] <- 0
INFY.LOGCUM[c(1:3, nrow(INFY.LOGCUM)),]

# Sum of all logarithmic Return

logcumret = sum(INFY.LOGCUM$INFY.LOG.RET, na.rm = TRUE)
logcumret

# Convert log return to arithmetic return

cumret = exp(logcumret) -1
cumret
# = 157%


# Clean up 

rm(list = ls())

# Comparing Price Return and Log Return

#data
library(xts)
library(quantmod)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)

# Closing price

INFY.PRC.RET <- data.INFY[,4]


# Calculate the Price Return 

INFY.PRC.RET$INFY.PRC.RET <- Delt(INFY.PRC.RET$Close)

head(INFY.PRC.RET)

#Logarithmic Total Return

# formula rt = ln(pt/Pt-1) = ln(1+ Rt) = ln Pt = ln Pt-1 ln = natural logrithum

# Calculate Log Returns

INFY.LOG.RET <- INFY.PRC.RET$Close

INFY.LOG.RET$INFY.LOG.RET <- diff(log(INFY.LOG.RET$Close))
head(INFY.LOG.RET)

# combine both return

tot.rets<- cbind(INFY.PRC.RET[,2], INFY.LOG.RET[,2])
head(tot.rets)

INFY.RET <- tot.rets
head(INFY.RET)

INFY.RET$INFY.PRC.RET[1] <-0
INFY.RET$INFY.LOG.RET[1] <-0
head(INFY.RET)

# Calculate the Gross return
INFY.RET$gross.prc <- 1+ INFY.RET$INFY.PRC.RET
INFY.RET$gross.log <- 1+ INFY.RET$INFY.LOG.RET

#Cumulate the gross return
INFY.RET$cum.prc <- cumprod(INFY.RET$gross.prc)
INFY.RET$cum.log <- cumprod(INFY.RET$gross.log)

head(INFY.RET)

#Plot

plot(INFY.RET$cum.log, 
     type = "l",
     auto.grid = FALSE,
     xlab = "Date", 
     ylabe = "Value of Investment (INR)",
     ylim = range(INFY.RET[, 5:6]),
     minor.ticks = FALSE,
     main = "Total Return with Log return" )

lines(INFY.RET$cum.prc, type = "l", lty = 3)
abline(h=1,col="black")
legend("topleft",col=c("black","black"),
       lty=c(1,3),
       c("Value Based on Total Return", "Value Based on Price Return"))

   