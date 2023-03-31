# TATA POWER Stock Analysis
library(xts)
library(quantmod)

#TATAPOWER

data.TATAPOWER = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/TATAPOWER.NS.csv", 
                          header = TRUE)

data.TATAPOWER$Date <-as.Date(data.TATAPOWER$Date, format = "%Y-%m-%d")
data.TATAPOWER <- data.TATAPOWER[order(data.TATAPOWER$Date), ]
data.TATAPOWER <- as.xts(data.TATAPOWER[, 2:7], order.by = data.TATAPOWER$Date)
names(data.TATAPOWER) = paste(c("TATAPOWER.OPEN", "TATAPOWER.HIGH", "TATAPOWER.LOW","TATAPOWER.CLOSE","TATAPOWER.ADJ.CLOSE","TATAPOWER.VOLUME"))
head(data.TATAPOWER)


#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)

# Combine data

multi <- data.TATAPOWER[,4]
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "TATAPOWER", "NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$TATAPOWER.idx <- multi.df$TATAPOWER/multi.df$TATAPOWER[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot
# Plotly
library(ggplot2)
library(plotly)

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$TATAPOWER.i,name = "TATAPOWER", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year TATAPOWER Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
