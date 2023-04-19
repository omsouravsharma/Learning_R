# How to see if stock is overvalue or undervalue 
# CAPM Capital Asset Pricing Model - Single factor
# Multi- factor model - FF Model 

# CAPM Regression 

# Rolling Widow Regression

# Libraries 
library(xts)
library(quantmod)
library(ggplot2)
library(plotly)
library(zoo)


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

data.RELIANCE <- result("RELIANCE")
data.NIFTY <- result("NSE")

REL <- data.RELIANCE[,4]
NIFTY <- data.NIFTY[,4]
# dim(REL)
# dim(NIFTY)
# as.numeric(REL)
# a = as.numeric(NIFTY)
# count(is.na(a))
# which(is.na(a))
# NIFTY[c(which(is.na(NIFTY)))]
# df <- merge(REL, NIFTY)


rets <- diff(log(REL))
rets$NIFTY<- diff(log(NIFTY))

names(rets)[1] <- "REL"
rets <- rets[-1,]
rets[c(1:3, nrow(rets)),]

#Create a rolling Window Regression Functions
coeffs <- rollapply(rets, width = 251, FUN = function(X){
  roll.reg = lm(REL~NIFTY, data = as.data.frame(X))
  return(roll.reg$coef)
},
by.column = FALSE)

coeffs<-rollapply(rets,
                     width=252,
                    FUN=function(X)
                       {
                         roll.reg=lm(REL~NIFTY,
                                       data=as.data.frame(X))
                         return(roll.reg$coef)
                         },
                     by.column=FALSE)

#Learn about rollapply function