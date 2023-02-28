# Comparing Capital Gains of Multiple Securities OverTime 
# TCS INFOSYS WIPRO NIFTY50
library(xts)

# Load data

data.TCS <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/TCS_5Y.csv", 
                     header = TRUE)
data.TCS$Date <-as.Date(data.TCS$Date, format = "%Y-%m-%d")
data.TCS <- data.TCS[order(data.TCS$Date), ]
data.TCS <- as.xts(data.TCS[, 2:7], order.by = data.TCS$Date)

names(data.TCS) <- paste(c("TCS.Open", "TCS.High", "TCS.Low","TCS.Close", 
                           "TCS.Adj.Close", "TCS.Volume"))

head(data.TCS)


#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY_5Y.csv", 
                     header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)

names(data.INFY) <- paste(c("INFY.Open", "INFY.High", "INFY.Low","INFY.Close", 
                           "INFY.Adj.Close", "INFY.Volume"))

head(data.INFY)


# WIPRO

data.WIPRO <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/WIPRO_5Y.csv", 
                     header = TRUE)
data.WIPRO$Date <-as.Date(data.WIPRO$Date, format = "%Y-%m-%d")
data.WIPRO <- data.WIPRO[order(data.WIPRO$Date), ]
data.WIPRO <- as.xts(data.WIPRO[, 2:7], order.by = data.WIPRO$Date)

names(data.WIPRO) <- paste(c("WIPRO.Open", "WIPRO.High", "WIPRO.Low","WIPRO.Close", 
                            "WIPRO.Adj.Close", "WIPRO.Volume"))

head(data.WIPRO)

#NSE
data.NSE <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE_5Y.csv", 
                     header = TRUE)
data.NSE$Date <-as.Date(data.NSE$Date, format = "%Y-%m-%d")
data.NSE <- data.NSE[order(data.NSE$Date), ]
data.NSE <- as.xts(data.NSE[, 2:7], order.by = data.NSE$Date)

names(data.NSE) <- paste(c("NSE.Open", "NSE.High", "NSE.Low","NSE.Close", 
                             "NSE.Adj.Close", "NSE.Volume"))

head(data.NSE)


# Combine data into one data object
close.price <- data.TCS$TCS.Close
close.price <- cbind(close.price, data.INFY$INFY.Close, 
                     data.WIPRO$WIPRO.Close, data.NSE$NSE.Close)

head(close.price)

# Convert data into a data.frame

multi.df <- cbind(index(close.price), data.frame(close.price))
names(multi.df) <- paste(c("date", "TCS", "INFY", "WIPRO", "NSE"))
rownames(multi.df) <- seq(1, nrow(multi.df), 1)
head(multi.df)

# Calculate Normalized Values for Each security.
multi.df$TCS.idx <- multi.df$TCS/multi.df$TCS[1]
multi.df$INFY.idx <- multi.df$INFY/multi.df$INFY[1]
multi.df$WIPRO.idx <- multi.df$WIPRO/multi.df$WIPRO[1]
multi.df$NSE.idx <- multi.df$NSE/multi.df$NSE[1]

head(multi.df)

# Plot
plot(x = multi.df$date,
     y = multi.df$NSE.idx,
     type = "l", 
     xlab = "Date", 
     ylab = "Value of Investment($)",
     ylim = range(multi.df[,6:9], na.rm=TRUE),
     col = "black", 
     lty = 1, 
     lwd = 2, 
     main = "Comparing NSE with TCS | INFY |WIPRO Last 5Y")

lines(x = multi.df$date, 
      y = multi.df$TCS.idx, col = "black",lty = 1, lwd = 2)

lines(x=multi.df$date,
      y=multi.df$INFY.idx, col="gray",
      lty=2,
      lwd=1)
lines(x=multi.df$date,
      y=multi.df$WIPRO.idx, col="gray",
      lty=1,
      lwd=1)

abline(h=1, lty = 2, col = "black")

legend("topleft", 
       c("NSE", "TCS", "INFY", "WIPRO"),
       col = c("black", "grey", "grey", "black"),
       lty=c(2,2,1,1),
       lwd=c(1,1,1,2))

#update color

# Plot
plot(x = multi.df$date,
     y = multi.df$NSE.idx,
     type = "l", 
     xlab = "Date", 
     ylab = "Value of Investment($)",
     ylim = range(multi.df[,6:9], na.rm=TRUE),
     col = "red", 
     lty = 1, 
     lwd = 2, 
     main = "Comparing NSE with TCS | INFY |WIPRO Last 5Y")

lines(x = multi.df$date, 
      y = multi.df$TCS.idx, col = "blue",lty = 1, lwd = 2)

lines(x=multi.df$date,
      y=multi.df$INFY.idx, col="violet",
      lty=2,
      lwd=2)
lines(x=multi.df$date,
      y=multi.df$WIPRO.idx, col="black",
      lty=1,
      lwd=1)

abline(h=1, lty = 2, col = "black")

legend("topleft", 
       c("NSE", "TCS", "INFY", "WIPRO"),
       col = c("red", "blue", "violet", "black"),
       lty=c(2,2,1,1),
       lwd=c(1,1,2,2))

library(ggplot2)
library(ggthemes)

myplot <-ggplot(data = multi.df, aes(x = multi.df$date))+
          geom_line(aes(y =multi.df$NSE.idx), color = 'red')+
          geom_line(aes(y =multi.df$TCS.idx), color = "blue")+
          geom_line(aes(y =multi.df$INFY.idx), color = 'darkgreen')+
          geom_line(aes(y =multi.df$WIPRO.idx), color = "goldenrod1")
 
myplot + theme_solarized_2() +geom_abline(h = 1, col = "black")

# PG 56
#https://rstudio-pubs-static.s3.amazonaws.com/419265_91c5fb9acf1742f88369c161ca59b6ef.html
rm(list = ls())
