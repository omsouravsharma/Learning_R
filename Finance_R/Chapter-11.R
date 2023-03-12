# Bank Stock

#Comparing Performance of Multiple Securities: Total Return
# VEDL, JINDALSTEL, JSWSTEEL, TATASTEEL, NIFTY50
rm(list = ls())
#LIBRARY
library(xts)
library(quantmod)


#VEDL

data.VEDL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/VEDL.NS.csv", 
                     header = TRUE)

data.VEDL$Date <-as.Date(data.VEDL$Date, format = "%Y-%m-%d")
data.VEDL <- data.VEDL[order(data.VEDL$Date), ]
data.VEDL <- as.xts(data.VEDL[, 2:7], order.by = data.VEDL$Date)
names(data.VEDL) = paste(c("VEDL.OPEN", "VEDL.HIGH", "VEDL.LOW","VEDL.CLOSE","VEDL.ADJ.CLOSE","VEDL.VOLUME"))
head(data.VEDL)

#JINDALSTEL
data.JINDALSTEL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/JINDALSTEL.NS.csv", 
                           header = TRUE)

data.JINDALSTEL$Date <-as.Date(data.JINDALSTEL$Date, format = "%Y-%m-%d")
data.JINDALSTEL <- data.JINDALSTEL[order(data.JINDALSTEL$Date), ]
data.JINDALSTEL <- as.xts(data.JINDALSTEL[, 2:7], order.by = data.JINDALSTEL$Date)
names(data.JINDALSTEL) = paste(c("JINDALSTEL.OPEN", "JINDALSTEL.HIGH", "JINDALSTEL.LOW","JINDALSTEL.CLOSE","JINDALSTEL.ADJ.CLOSE","JINDALSTEL.VOLUME"))
head(data.JINDALSTEL)

#JSWSTEEL
data.JSWSTEEL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/JSWSTEEL.NS.csv", 
                         header = TRUE)

data.JSWSTEEL$Date <-as.Date(data.JSWSTEEL$Date, format = "%Y-%m-%d")
data.JSWSTEEL <- data.JSWSTEEL[order(data.JSWSTEEL$Date), ]
data.JSWSTEEL <- as.xts(data.JSWSTEEL[, 2:7], order.by = data.JSWSTEEL$Date)
names(data.JSWSTEEL) = paste(c("JSWSTEEL.OPEN", "JSWSTEEL.HIGH", "JSWSTEEL.LOW","JSWSTEEL.CLOSE","JSWSTEEL.ADJ.CLOSE","JSWSTEEL.VOLUME"))
head(data.JSWSTEEL)

#TATASTEEL

data.TATASTEEL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/TATASTEEL.NS.csv", 
                          header = TRUE)

data.TATASTEEL$Date <-as.Date(data.TATASTEEL$Date, format = "%Y-%m-%d")
data.TATASTEEL <- data.TATASTEEL[order(data.TATASTEEL$Date), ]
data.TATASTEEL <- as.xts(data.TATASTEEL[, 2:7], order.by = data.TATASTEEL$Date)
names(data.TATASTEEL) = paste(c("TATASTEEL.OPEN", "TATASTEEL.HIGH", "TATASTEEL.LOW","TATASTEEL.CLOSE","TATASTEEL.ADJ.CLOSE","TATASTEEL.VOLUME"))
head(data.TATASTEEL)

#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)


# Combine data

multi <- data.VEDL[,4]
multi<-merge(multi,data.JINDALSTEL[,4])
multi<-merge(multi,data.JSWSTEEL[,4])
multi<-merge(multi,data.TATASTEEL[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "VEDL", "JINDALSTEL","JSWSTEEL", "TATASTEEL","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$VEDL.idx <- multi.df$VEDL/multi.df$VEDL[1]
multi.df$JINDALSTEL.idx <- multi.df$JINDALSTEL/multi.df$JINDALSTEL[1]
multi.df$JSWSTEEL.idx <- multi.df$JSWSTEEL/multi.df$JSWSTEEL[1]
multi.df$TATASTEEL.idx <- multi.df$TATASTEEL/multi.df$TATASTEEL[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot

plot(multi.df$Date, 
     xlab = "Date",
     y = multi.df$VEDL.idx, 
     #     ylim = range(multi.df[,6:7]), 
     ylab = "Value of 1 INR", 
     type = "l",
     col = "red", 
     lty =1,
     lwd = 2, 
     main = "Value of 1 INR invested")
lines(x=multi.df$Date,
      y=multi.df$JINDALSTEL.idx,
      col="blue",
      lty=2,
      lwd=1)
lines(x=multi.df$Date,
      y=multi.df$JSWSTEEL.idx,
      col="violet",
      lty=1,
      lwd=2)
lines(x=multi.df$Date,
      y=multi.df$TATASTEEL.idx,
      col="black",
      lty=1,
      lwd=2)
lines(x=multi.df$Date,
      y=multi.df$NIFTY.idx,
      col="green",
      lty=1,
      lwd=1)
abline(h=1,lty=1,col="black")
legend("topleft",
       c("VEDL","JINDALSTEL","JSWSTEEL","TATSTEEL","NIFTY50"),
       col=c("red","blue","black","violet","green"),
       lty=c(1,1,1,1),
       lwd=c(1,2,1,2))

# Plotly
library(ggplot2)

library(plotly)
fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$VEDL.idx,name = "VEDL", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$JINDALSTEL.idx, name= "JINDALSTEL")
fig <- fig %>% add_trace(y = multi.df$JSWSTEEL.idx, name= "JSWSTEEL")
fig <- fig %>% add_trace(y = multi.df$TATASTEEL.idx, name= "TATASETEEL")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year Metal Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig

