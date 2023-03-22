# Battery Stock Analysis 5 Year yields 

#Comparing Performance of Multiple Securities: Total Return
# EXIDE, AMARAJA, HBL, NIFTY50
rm(list = ls())
#LIBRARY
library(xts)
library(quantmod)


#EXIDE

data.EXIDE = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/EXIDEIND.NS.csv", 
                      header = TRUE)

data.EXIDE$Date <-as.Date(data.EXIDE$Date, format = "%Y-%m-%d")
data.EXIDE <- data.EXIDE[order(data.EXIDE$Date), ]
data.EXIDE <- as.xts(data.EXIDE[, 2:7], order.by = data.EXIDE$Date)
names(data.EXIDE) = paste(c("EXIDE.OPEN", "EXIDE.HIGH", "EXIDE.LOW","EXIDE.CLOSE","EXIDE.ADJ.CLOSE","EXIDE.VOLUME"))
head(data.EXIDE)

#HBLPOWER
data.HBLPOWER = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/HBLPOWER.NS.csv", 
                         header = TRUE)

data.HBLPOWER$Date <-as.Date(data.HBLPOWER$Date, format = "%Y-%m-%d")
data.HBLPOWER <- data.HBLPOWER[order(data.HBLPOWER$Date), ]
data.HBLPOWER <- as.xts(data.HBLPOWER[, 2:7], order.by = data.HBLPOWER$Date)
names(data.HBLPOWER) = paste(c("HBLPOWER.OPEN", "HBLPOWER.HIGH", "HBLPOWER.LOW","HBLPOWER.CLOSE","HBLPOWER.ADJ.CLOSE","HBLPOWER.VOLUME"))
head(data.HBLPOWER)

#AMARAJABAT
data.AMARAJABAT = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/AMARAJABAT.NS.csv", 
                           header = TRUE)

data.AMARAJABAT$Date <-as.Date(data.AMARAJABAT$Date, format = "%Y-%m-%d")
data.AMARAJABAT <- data.AMARAJABAT[order(data.AMARAJABAT$Date), ]
data.AMARAJABAT <- as.xts(data.AMARAJABAT[, 2:7], order.by = data.AMARAJABAT$Date)
names(data.AMARAJABAT) = paste(c("AMARAJABAT.OPEN", "AMARAJABAT.HIGH", "AMARAJABAT.LOW","AMARAJABAT.CLOSE","AMARAJABAT.ADJ.CLOSE","AMARAJABAT.VOLUME"))
head(data.AMARAJABAT)

#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)


# Combine data

multi <- data.EXIDE[,4]
multi<-merge(multi,data.HBLPOWER[,4])
multi<-merge(multi,data.AMARAJABAT[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "EXIDE", "HBLPOWER","AMARAJABAT","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$EXIDE.idx <- multi.df$EXIDE/multi.df$EXIDE[1]
multi.df$HBLPOWER.idx <- multi.df$HBLPOWER/multi.df$HBLPOWER[1]
multi.df$AMARAJABAT.idx <- multi.df$AMARAJABAT/multi.df$AMARAJABAT[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)


# Plotly
library(ggplot2)
library(plotly)

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$EXIDE.idx,name = "EXIDE", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$HBLPOWER.idx, name= "HBLPOWER")
fig <- fig %>% add_trace(y = multi.df$AMARAJABAT.idx, name= "AMARAJABAT")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year Battery Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig

