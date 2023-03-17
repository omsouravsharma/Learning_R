# FMCG Stock Analysis
library(xts)
library(quantmod)
#DABUR

data.DABUR = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/DABUR.NS.csv", 
                      header = TRUE)

data.DABUR$Date <-as.Date(data.DABUR$Date, format = "%Y-%m-%d")
data.DABUR <- data.DABUR[order(data.DABUR$Date), ]
data.DABUR <- as.xts(data.DABUR[, 2:7], order.by = data.DABUR$Date)
names(data.DABUR) = paste(c("DABUR.OPEN", "DABUR.HIGH", "DABUR.LOW","DABUR.CLOSE","DABUR.ADJ.CLOSE","DABUR.VOLUME"))
head(data.DABUR)

#TATACONSUM

data.TATACONSUM = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/TATACONSUM.NS.csv", 
                           header = TRUE)

data.TATACONSUM$Date <-as.Date(data.TATACONSUM$Date, format = "%Y-%m-%d")
data.TATACONSUM <- data.TATACONSUM[order(data.TATACONSUM$Date), ]
data.TATACONSUM <- as.xts(data.TATACONSUM[, 2:7], order.by = data.TATACONSUM$Date)
names(data.TATACONSUM) = paste(c("TATACONSUM.OPEN", "TATACONSUM.HIGH", "TATACONSUM.LOW","TATACONSUM.CLOSE","TATACONSUM.ADJ.CLOSE","TATACONSUM.VOLUME"))
head(data.TATACONSUM)

#NESTLEIND

data.NESTLEIND = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NESTLEIND.NS.csv", 
                          header = TRUE)

data.NESTLEIND$Date <-as.Date(data.NESTLEIND$Date, format = "%Y-%m-%d")
data.NESTLEIND <- data.NESTLEIND[order(data.NESTLEIND$Date), ]
data.NESTLEIND <- as.xts(data.NESTLEIND[, 2:7], order.by = data.NESTLEIND$Date)
names(data.NESTLEIND) = paste(c("NESTLEIND.OPEN", "NESTLEIND.HIGH", "NESTLEIND.LOW","NESTLEIND.CLOSE","NESTLEIND.ADJ.CLOSE","NESTLEIND.VOLUME"))
head(data.NESTLEIND)

#ITC

data.ITC = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/ITC.NS.csv", 
                    header = TRUE)

data.ITC$Date <-as.Date(data.ITC$Date, format = "%Y-%m-%d")
data.ITC <- data.ITC[order(data.ITC$Date), ]
data.ITC <- as.xts(data.ITC[, 2:7], order.by = data.ITC$Date)
names(data.ITC) = paste(c("ITC.OPEN", "ITC.HIGH", "ITC.LOW","ITC.CLOSE","ITC.ADJ.CLOSE","ITC.VOLUME"))
head(data.ITC)

#HUL
data.HINDUNILVR = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/HINDUNILVR.NS.csv", 
                           header = TRUE)

data.HINDUNILVR$Date <-as.Date(data.HINDUNILVR$Date, format = "%Y-%m-%d")
data.HINDUNILVR <- data.HINDUNILVR[order(data.HINDUNILVR$Date), ]
data.HINDUNILVR <- as.xts(data.HINDUNILVR[, 2:7], order.by = data.HINDUNILVR$Date)
names(data.HINDUNILVR) = paste(c("HINDUNILVR.OPEN", "HINDUNILVR.HIGH", "HINDUNILVR.LOW","HINDUNILVR.CLOSE","HINDUNILVR.ADJ.CLOSE","HINDUNILVR.VOLUME"))
head(data.HINDUNILVR)


#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)

# Combine data

multi <- data.DABUR[,4]
multi<-merge(multi,data.HINDUNILVR[,4])
multi<-merge(multi,data.ITC[,4])
multi<-merge(multi,data.NESTLEIND[,4])
multi<-merge(multi,data.TATACONSUM[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "DABUR", "HINDUILVR","ITC","NESTLEIND", "TATACONSUM","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$DABUR.idx <- multi.df$DABUR/multi.df$DABUR[1]
multi.df$HINDUILVR.idx <- multi.df$HINDUILVR/multi.df$HINDUILVR[1]
multi.df$ITC.idx <- multi.df$ITC/multi.df$ITC[1]
multi.df$NESTLEIND.idx <- multi.df$NESTLEIND/multi.df$NESTLEIND[1]
multi.df$TATACONSUM.idx <- multi.df$TATACONSUM/multi.df$TATACONSUM[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot

plot(multi.df$Date, 
     xlab = "Date",
     y = multi.df$DABUR.idx, 
     #  ylim = range(multi.df[,6:9]), 
     ylab = "Value of 1 INR", 
     type = "l",
     col = "red", 
     lty =1,
     lwd = 2, 
     main = "Value of 1 INR invested")
lines(x=multi.df$Date,
      y=multi.df$HINDUILVR.idx,
      col="blue",
      lty=2,
      lwd=1)
lines(x=multi.df$Date,
      y=multi.df$ITC.idx,
      col="violet",
      lty=1,
      lwd=2)
lines(x=multi.df$Date,
      y=multi.df$NESTLEIND.idx,
      col="yellow",
      lty=1,
      lwd=2)
lines(x=multi.df$Date,
      y=multi.df$TATACONSUM.idx,
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
fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$DABUR.idx,name = "DABUR", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$HINDUILVR.idx, name= "HUL")
fig <- fig %>% add_trace(y = multi.df$ITC.idx, name= "ITC")
fig <- fig %>% add_trace(y = multi.df$NESTLEIND.idx, name= "NESTLE")
fig <- fig %>% add_trace(y = multi.df$TATACONSUM.idx, name= "TATACONSUM")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year FMCG Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
