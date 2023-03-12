# Bank Stock

#Comparing Performance of Multiple Securities: Total Return
# ICICI, HDFC, SBI, NIFTY50

#LIBRARY
library(xts)
library(quantmod)
rm(list = ls())

#ICICI

data.ICICI = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/ICICIBANK.NS.csv", 
                      header = TRUE)

data.ICICI$Date <-as.Date(data.ICICI$Date, format = "%Y-%m-%d")
data.ICICI <- data.ICICI[order(data.ICICI$Date), ]
data.ICICI <- as.xts(data.ICICI[, 2:7], order.by = data.ICICI$Date)
names(data.ICICI) = paste(c("ICICI.OPEN", "ICICI.HIGH", "ICICI.LOW","ICICI.CLOSE","ICICI.ADJ.CLOSE","ICICI.VOLUME"))
head(data.ICICI)

#HDFC
data.HDFC = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/HDFCBANK.NS.csv", 
                     header = TRUE)

data.HDFC$Date <-as.Date(data.HDFC$Date, format = "%Y-%m-%d")
data.HDFC <- data.HDFC[order(data.HDFC$Date), ]
data.HDFC <- as.xts(data.HDFC[, 2:7], order.by = data.HDFC$Date)
names(data.HDFC) = paste(c("HDFC.OPEN", "HDFC.HIGH", "HDFC.LOW","HDFC.CLOSE","HDFC.ADJ.CLOSE","HDFC.VOLUME"))
head(data.HDFC)

#SBI
data.SBI= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/SBIN.NS.csv", 
                     header = TRUE)
data.SBI$Date <-as.Date(data.SBI$Date, format = "%Y-%m-%d")
data.SBI <- data.SBI[order(data.SBI$Date), ]
data.SBI <- as.xts(data.SBI[, 2:7], order.by = data.SBI$Date)
names(data.SBI) = paste(c("SBI.OPEN", "SBI.HIGH", "SBI.LOW","SBI.CLOSE","SBI.ADJ.CLOSE","SBI.VOLUME"))
head(data.SBI)

#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)


# Combine data

multi <- data.ICICI[,4]
multi<-merge(multi,data.HDFC[,4])
multi<-merge(multi,data.SBI[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "ICICI", "HDFC","SBI", "NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$ICICI.idx <- multi.df$ICICI/multi.df$ICICI[1]
multi.df$HDFC.idx <- multi.df$HDFC/multi.df$HDFC[1]
multi.df$SBI.idx <- multi.df$SBI/multi.df$SBI[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)

range(multi.df[,(5)])

#Plot

plot(multi.df$Date, 
     xlab = "Date",
     y = multi.df$ICICI.idx, 
#     ylim = range(multi.df[,6:7]), 
     ylab = "Value of 1 INR", 
     type = "l",
     col = "red", 
     lty =1,
     lwd = 2, 
     main = "Value of 1 INR invested")
lines(x=multi.df$Date,
      y=multi.df$HDFC.idx,
      col="blue",
      lty=2,
      lwd=1)
lines(x=multi.df$Date,
      y=multi.df$SBI.idx,
      col="violet",
      lty=1,
      lwd=2)
lines(x=multi.df$Date,
      y=multi.df$NIFTY.idx,
      col="green",
      lty=1,
      lwd=1)
abline(h=1,lty=1,col="black")
legend("topleft",
       c("ICICI","HDFC","SBI","NIFTY50"),
       col=c("red","blue","violet","green"),
       lty=c(1,1,1,1),
       lwd=c(1,2,1,2))

# Plotly
library(ggplot2)

library(plotly)
fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$ICICI.idx,name = "ICICI", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$HDFC.idx, name= "HDFC")
fig <- fig %>% add_trace(y = multi.df$SBI.idx, name= "SBI")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year Bank Share Performance ICICI vs HDFC vs SBI vs NIFTY",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig

