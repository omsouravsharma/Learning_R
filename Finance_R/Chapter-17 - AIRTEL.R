# Airtel Stock Analysis
library(xts)
library(quantmod)
#AIRTEL

data.BHARTIARTL = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/BHARTIARTL.NS.csv", 
                           header = TRUE)

data.BHARTIARTL$Date <-as.Date(data.BHARTIARTL$Date, format = "%Y-%m-%d")
data.BHARTIARTL <- data.BHARTIARTL[order(data.BHARTIARTL$Date), ]
data.BHARTIARTL <- as.xts(data.BHARTIARTL[, 2:7], order.by = data.BHARTIARTL$Date)
names(data.BHARTIARTL) = paste(c("BHARTIARTL.OPEN", "BHARTIARTL.HIGH", "BHARTIARTL.LOW","BHARTIARTL.CLOSE","BHARTIARTL.ADJ.CLOSE","BHARTIARTL.VOLUME"))
head(data.BHARTIARTL)


#NIFTY
data.NIFTY= read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE.csv", 
                     header = TRUE)
data.NIFTY$Date <-as.Date(data.NIFTY$Date, format = "%Y-%m-%d")
data.NIFTY <- data.NIFTY[order(data.NIFTY$Date), ]
data.NIFTY <- as.xts(data.NIFTY[, 2:7], order.by = data.NIFTY$Date)
names(data.NIFTY) = paste(c("NIFTY.OPEN", "NIFTY.HIGH", "NIFTY.LOW","NIFTY.CLOSE","NIFTY.ADJ.CLOSE","NIFTY.VOLUME"))
head(data.NIFTY)

# Combine data

multi <- data.BHARTIARTL[,4]
multi<-merge(multi,data.NIFTY[,4])

head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "BHARTIARTL", "NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$BHARTIARTL.idx <- multi.df$BHARTIARTL/multi.df$BHARTIARTL[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot
# Plotly
library(ggplot2)
library(plotly)

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$BHARTIARTL.idx,name = "BHARTIARTL", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year BHARTIARTL Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
