# PSU Stock Analysis
library(xts)
library(quantmod)

#NTPC

data.NTPC = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NTPC.NS.csv", 
                     header = TRUE)

data.NTPC$Date <-as.Date(data.NTPC$Date, format = "%Y-%m-%d")
data.NTPC <- data.NTPC[order(data.NTPC$Date), ]
data.NTPC <- as.xts(data.NTPC[, 2:7], order.by = data.NTPC$Date)
names(data.NTPC) = paste(c("NTPC.OPEN", "NTPC.HIGH", "NTPC.LOW","NTPC.CLOSE","NTPC.ADJ.CLOSE","NTPC.VOLUME"))
head(data.NTPC)


#GAIL

data.GAIL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/GAIL.NS.csv", 
                     header = TRUE)

data.GAIL$Date <-as.Date(data.GAIL$Date, format = "%Y-%m-%d")
data.GAIL <- data.GAIL[order(data.GAIL$Date), ]
data.GAIL <- as.xts(data.GAIL[, 2:7], order.by = data.GAIL$Date)
names(data.GAIL) = paste(c("GAIL.OPEN", "GAIL.HIGH", "GAIL.LOW","GAIL.CLOSE","GAIL.ADJ.CLOSE","GAIL.VOLUME"))
head(data.GAIL)

#IOC

data.IOC = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/IOC.NS.csv", 
                    header = TRUE)

data.IOC$Date <-as.Date(data.IOC$Date, format = "%Y-%m-%d")
data.IOC <- data.IOC[order(data.IOC$Date), ]
data.IOC <- as.xts(data.IOC[, 2:7], order.by = data.IOC$Date)
names(data.IOC) = paste(c("IOC.OPEN", "IOC.HIGH", "IOC.LOW","IOC.CLOSE","IOC.ADJ.CLOSE","IOC.VOLUME"))
head(data.IOC)
#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)

# Combine data

multi <- data.NTPC[,4]
multi<-merge(multi,data.GAIL[,4])
multi<-merge(multi,data.IOC[,4])
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "NTPC", "GAIL","IOC","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$NTPC.idx <- multi.df$NTPC/multi.df$NTPC[1]
multi.df$GAIL.idx <- multi.df$GAIL/multi.df$GAIL[1]
multi.df$IOC.idx <- multi.df$IOC/multi.df$IOC[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot
# Plotly
library(ggplot2)
library(plotly)

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$NTPC.idx,name = "NTPC", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$GAIL.idx, name= "GAIL")
fig <- fig %>% add_trace(y = multi.df$IOC.idx, name= "IOC")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year PSU Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
