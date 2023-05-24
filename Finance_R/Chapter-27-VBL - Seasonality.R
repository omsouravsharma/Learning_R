# Stock Return calculator 

# Libraries 
library(xts)
library(quantmod)
library(ggplot2)
library(plotly)
library(fpp3)

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

data.VBL <- result("VBL")

data.VBL_raw <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/VBL.NS.csv", sep = ',', header = TRUE)
data.VBL_raw$Date <-as.Date(data.VBL_raw$Date, format = "%Y-%m-%d")
# data.VBL_raw$Date <- data.VBL_raw[order(data.VBL_raw$Date), ]
# data.VBL_raw <- data.VBL_raw[order(data.VBL_raw$Date), ]
# 
# data.VBL_raw<- as.xts(data.VBL_raw[, 2:7], order.by = data.VBL_raw$Date)

data.VBL_raw

class(data.VBL_raw)

ts_vbl<- as_tsibble(data.VBL_raw, index = Date, regular = TRUE)
ts_vbl

#auto-plot
ts_vbl|>
  autoplot(Close)

#filling gaps with the previous value. 
ts_vbl|>
  tsibble::fill_gaps()|>
  fill(Open,High, Low, Close, Adj.Close, Volume, .direction = "down")->ts_vbl

gg_season(ts_vbl, y = Close, period = 31)
# gg_subseries(ts_vbl)  

?gg_season
# 
# https://www.rpubs.com/AurelliaChristie/time-series-and-stock-analysis
# https://anomaly.io/seasonal-trend-decomposition-in-r/index.html
# 
#   

# Check seasonality in NIFTY 



NIFTY_MON <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/NSE_MONTHLY.NS.csv", sep = ',', header = TRUE)
NIFTY_MON$Date <-as.Date(NIFTY_MON$Date, format = "%Y-%m-%d")
NIFTY_MON

class(NIFTY_MON)

ts_NIFTY_MON<- as_tsibble(NIFTY_MON, index = Date, regular = TRUE)
ts_NIFTY_MON

#auto-plot
ts_NIFTY_MON|>
  autoplot(Close)

#filling gaps with the previous value. 
ts_NIFTY_MON|>
  tsibble::fill_gaps()|>
  fill(Open,High, Low, Close, Adj.Close, Volume, .direction = "down")->ts_NIFTY_MON

gg_season(ts_NIFTY_MON, y = Close, facet_period = 30)
gg_subseries(ts_NIFTY_MON, y = Close)  

library(tsibble)
library(dplyr)
ts_NIFTY_MON %>%
  gg_season(Close)
