# Multiple asset portfolio 
#REL #ICICI #BHARTI #INFY

# Libraries 
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

data.RELIANCE <- result("RELIANCE")
data.ICICIBANK <- result("ICICIBANK")
data.AIRTEL <- result("BHARTIARTL")
data.INFY <- result("INFY")

multi <- data.RELIANCE[,4]
multi<-merge(multi,data.ICICIBANK[,4])
multi<-merge(multi,data.AIRTEL[,4])
multi<-merge(multi,data.INFY[,4])
head(multi)

#Calculate the return of each Security. 

mat.price <- matrix(multi, nrow(multi))
prc2ret <- function(x) Delt(x)

mat.ret <- apply(mat.price,2, function(x){prc2ret(c(x))})
mat.ret[1:4,]

mat.ret <- mat.ret[-1,]
mat.ret[1:4,]

colnames(mat.ret) <- c("REL","ICICI","AIRTEL","INFY")
mat.ret[1:4,]

#Calculate Ammualized Variance-Covariance Matrix

VCOV <- cov(mat.ret)
VCOV

#Annualized the VCOV
VCOV.annual <- 252*VCOV 
VCOV.annual

wgt <- c(.2,.2,.3,.3)
mat.wgt <-matrix(wgt,1)
mat.wgt

tmat.wgt <- t(mat.wgt)
tmat.wgt

#Calculate the Portfolio Variance 
port.var <- mat.wgt %*% VCOV.annual %*% tmat.wgt
port.var[1,1]

#Standard Deviation
port.sd <- sqrt(port.var)
port.sd
