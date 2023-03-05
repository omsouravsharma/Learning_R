# Momentum 
# Relative Strength Index RSI

#RSI = 100 - (100/1+RS)

#data
library(xts)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)
INFY.RSI <- data.INFY[,4]
INFY.RSI$delta <- diff(INFY.RSI$Close)
head(data.INFY)

head(INFY.RSI)

# Create a dummy variable to Indicate whether price went up or price went down

INFY.RSI$up <- ifelse(INFY.RSI$delta > 0,1, 0)
INFY.RSI$down <- ifelse(INFY.RSI$delta < 0,1, 0)

head(INFY.RSI)

# Calculate price for up days and price for down days

INFY.RSI$up.val <- INFY.RSI$delta*INFY.RSI$up
INFY.RSI$down.val <- -INFY.RSI$delta*INFY.RSI$down

head(INFY.RSI)

# Calculate the initial up and down 14 day avergae

INFY.RSI$up.first.av <- rollapply(INFY.RSI$up.val,
                                  width = 14, 
                                  FUN = mean, fill = NA, na.rm = TRUE)

INFY.RSI$down.first.av <- rollapply(INFY.RSI$down.val,
                                  width = 14, 
                                  FUN = mean, fill = NA, na.rm = TRUE)

INFY.RSI[c(1:16),]

# Calculate the Wilder Exponential Moving Avergae 


up.val <- as.numeric(INFY.RSI$up.val)
down.val <- as.numeric(INFY.RSI$down.val)

INFY.RSI$up.avg <- INFY.RSI$up.first.av
for (i in 15:nrow(INFY.RSI)) {
  INFY.RSI$up.avg[i] <- ((INFY.RSI$up.avg[i-1]*13+up.val[i])/14)
}

INFY.RSI$down.avg <- INFY.RSI$down.first.av
for (i in 15:nrow(INFY.RSI)) {
  INFY.RSI$down.avg[i] <- ((INFY.RSI$down.avg[i-1]*13+down.val[i])/14)
}

INFY.RSI[c(1:16),]


# Calculate Average 

INFY.RSI$RS<- INFY.RSI$up.avg/INFY.RSI$down.avg

INFY.RSI$RSI <- 100- (100/(1+INFY.RSI$RS))

INFY.RSI[c(1:16),]

# Plot RSI


plot(x=index(INFY.RSI),
       xlab="Date",
       y=INFY.RSI$RSI,
       ylab="RSI (14-Day Moving Average)",
       ylim=c(0,100),
       type="l",
       main="INFY - Relative Strength Index")
abline(h=c(30,70),lty=2)

###
library(quantmod)
chartSeries(data.INFY[,4],
            theme="white.mono",
            TA=c(addSMA(n=c(50,200))))

?chartSeries
