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
class(result("IOC"))
head(result)


data.SCHNEIDER <- result("SCHNEIDER")
data.CDSL <- result("CDSL")
data.BEL <- result("BEL")
data.TATAMOTORS <- result("TATAMOTORS")
data.ACC <- result("ACC")
data.CIPLA <- result("CIPLA")
data.SUNPHARMA <- result("SUNPHARMA")
data.NIFTY <- result("NSE")


multi <- data.SCHNEIDER[,4]
multi<-merge(multi,data.CDSL[,4])
multi<-merge(multi,data.BEL[,4])
multi<-merge(multi,data.TATAMOTORS[,4])
multi<-merge(multi,data.ACC[,4])
multi<-merge(multi,data.CIPLA[,4])
multi<-merge(multi,data.SUNPHARMA[,4])
multi<-merge(multi,data.NIFTY[,4])
head(multi)

# Convert data into data.frame Object
multi.df <- cbind(data.frame(index(multi), data.frame(multi)))
names(multi.df)<- paste(c("Date", "SCHNEIDER","CDSL","BEL","TATAMOTORS","ACC","CIPLA","SUNPHARMA","NIFTY"))
head(multi.df)

#Constructing normalized value for each security

multi.df$SCHNEIDER.idx <- multi.df$SCHNEIDER/multi.df$SCHNEIDER[1]
multi.df$CDSL.idx <- multi.df$CDSL/multi.df$CDSL[1]
multi.df$BEL.idx <- multi.df$BEL/multi.df$BEL[1]
multi.df$TATAMOTORS.idx <- multi.df$TATAMOTORS/multi.df$TATAMOTORS[1]
multi.df$ACC.idx <- multi.df$ACC/multi.df$ACC[1]
multi.df$CIPLA.idx <- multi.df$CIPLA/multi.df$CIPLA[1]
multi.df$SUNPHARMA.idx <- multi.df$SUNPHARMA/multi.df$SUNPHARMA[1]
multi.df$NIFTY.idx <- multi.df$NIFTY/multi.df$NIFTY[1]
head(multi.df)



#Plot

fig <- plot_ly(multi.df, x = multi.df$Date, y=multi.df$SCHNEIDER.idx,name = "SCHNEIDER", type = 'scatter', mode= 'lines')
fig <- fig %>% add_trace(y = multi.df$CDSL.idx, name= "CDSL")
fig <- fig %>% add_trace(y = multi.df$BEL.idx, name= "BEL")
fig <- fig %>% add_trace(y = multi.df$TATAMOTORS.idx, name= "TATAMOTORS")
fig <- fig %>% add_trace(y = multi.df$ACC.idx, name= "ACC")
fig <- fig %>% add_trace(y = multi.df$CIPLA.idx, name= "CIPLA")
fig <- fig %>% add_trace(y = multi.df$SUNPHARMA.idx, name= "SUNPHARMA")
fig <- fig %>% add_trace(y = multi.df$NIFTY.idx, name= "NIFTY")
fig <- fig %>% layout(title = "5 Year MULTIPLE Share Performance ",
                      xaxis = list(title = "Dates"),
                      yaxis = list (title = "1 INR Value"))
fig
