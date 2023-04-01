# Automate 
library(xts)
library(quantmod)
library(ggplot2)
library(plotly)


result <- function(STOCK_TICK){
  filename = paste("C:/Users/NEXT/Desktop/Learning_R/Data/stock/",STOCK_TICK,".NS.csv", sep = '')
  data.STOCK_TICK = read.csv(filename, header = TRUE)
  
  data.STOCK_TICK$Date <-as.Date(data.STOCK_TICK$Date, format = "%Y-%m-%d")
  data.STOCK_TICK <- data.STOCK_TICK[order(data.STOCK_TICK$Date), ]
  data.STOCK_TICK <- as.xts(data.STOCK_TICK[, 2:7], order.by = data.STOCK_TICK$Date)
  col_names <- ""
  stock_col_name = strsplit(paste0(STOCK_TICK,".OPEN"," ", STOCK_TICK,".HIGH"," ", STOCK_TICK,".LOW"," ",STOCK_TICK,".CLOSE"," ",STOCK_TICK,".ADJ.CLOSE"," ",STOCK_TICK,".VOLUME"),split =" ")
  for(i in stock_col_name){col_names<- c(i)}
  names(data.STOCK_TICK) =col_names
  return(data.STOCK_TICK)  
}


data.SHREECEM <- result("SHREECEM")
data.JKCEMENT <- result("JKCEMENT")
data.ACC <- result("ACC")
data.NIFTY <- result("NSE")


multi <- data.SHREECEM[,4]
multi<-merge(multi,data.JKCEMENT[,4])
multi<-merge(multi,data.ACC[,4])
multi<-merge(multi,data.NIFTY[,4])
head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "SHREECEM","JKCEMENT","ACC","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$SHREECEM.idx <- multi.df$SHREECEM/multi.df$SHREECEM[1]
multi.df$JKCEMENT.idx <- multi.df$JKCEMENT/multi.df$JKCEMENT[1]
multi.df$ACC.idx <- multi.df$ACC/multi.df$ACC[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$SHREECEM.idx,name = "SHREECEM", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$JKCEMENT.idx, name= "JKCEMENT")
fig <- fig %>% add_trace(y = multi.df$ACC.idx, name= "ACC")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year CEMENT Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
