#Comparing Performance of Multiple Securities: Total Return
# INFY, TCS, WIPRO, NIFTY50

#LIBRARY
library(xts)
library(quantmod)
rm(list = ls())

#INFY

data.INFY = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY.NS.csv", 
                     header = TRUE)

data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)
names(data.INFY) = paste(c("INFY.OPEN", "INFY.HIGH", "INFY.LOW","INFY.CLOSE","INFY.ADJ.CLOSE","INFY.VOLUME"))
head(data.INFY)

#TCS
data.TCS= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/TCS.NS.csv", 
                   header = TRUE)
data.TCS$Date <-as.Date(data.TCS$Date, format = "%Y-%m-%d")
data.TCS <- data.TCS[order(data.TCS$Date), ]
data.TCS <- as.xts(data.TCS[, 2:7], order.by = data.TCS$Date)
names(data.TCS) = paste(c("TCS.OPEN", "TCS.HIGH", "TCS.LOW","TCS.CLOSE","TCS.ADJ.CLOSE","TCS.VOLUME"))
head(data.TCS)

#WIPRO
data.WIPRO= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/WIPRO.NS.csv", 
                     header = TRUE)
data.WIPRO$Date <-as.Date(data.WIPRO$Date, format = "%Y-%m-%d")
data.WIPRO <- data.WIPRO[order(data.WIPRO$Date), ]
data.WIPRO <- as.xts(data.WIPRO[, 2:7], order.by = data.WIPRO$Date)
names(data.WIPRO) = paste(c("WIPRO.OPEN", "WIPRO.HIGH", "WIPRO.LOW","WIPRO.CLOSE","WIPRO.ADJ.CLOSE","WIPRO.VOLUME"))
head(data.WIPRO)

#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)


# Combine data

multi <- data.INFY[,4]
multi<-merge(multi,data.TCS[,4])
multi<-merge(multi,data.WIPRO[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "INFY", "TCS","WIPRO", "NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$INFY.idx <- multi.df$INFY/multi.df$INFY[1]
multi.df$TCS.idx <- multi.df$TCS/multi.df$TCS[1]
multi.df$WIPRO.idx <- multi.df$WIPRO/multi.df$WIPRO[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df[])


#Plot

plot(multi.df$Date, 
     xlab = "Date",
     y = multi.df$INFY.idx, 
     ylim = range(multi.df[,6:8]), 
     ylab = "Value of 1 INR", 
     type = "l",
     col = "red", 
     lty =1,
     lwd = 2, 
     main = "Value of 1 INR invested")
lines(x=multi.df$Date,
        y=multi.df$TCS.idx,
         col="blue",
         lty=2,
         lwd=1)
lines(x=multi.df$Date,
         y=multi.df$WIPRO.idx,
         col="violet",
         lty=1,
         lwd=2)
lines(x=multi.df$Date,
         y=multi.df$NIFTY.idx,
         col="green",
         lty=1,
         lwd=1)
abline(h=3,lty=1,col="black")
legend("topleft",
          c("INFY","TCS","WIPRO","NIFTY50"),
         col=c("red","blue","violet","green"),
         lty=c(1,1,1,1),
         lwd=c(1,2,1,2))

# Plotly
library(ggplot2)

library(plotly)
fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$INFY.idx,name = "INFY", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$TCS.idx, name= "TCS")
fig <- fig %>% add_trace(y = multi.df$WIPRO.idx, name= "WIPRO")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
#fig <- fig %>% add_lines(y = 1, line = list(color = "grey"))
fig <- fig %>% layout(title = "5 Year IT Share Performance INFY vs TCS vs WIPRO vs NIFTY",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig

