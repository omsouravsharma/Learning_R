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

# Investment made at 2018 would return 157% in 2023


