# VAR - Value at Risk
# Historical VaR - Follow normal distribution

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

REL.ret <- Delt(data.RELIANCE$RELIANCE.ADJ.CLOSE)
ICICI.ret <- Delt(data.ICICIBANK$ICICIBANK.ADJ.CLOSE)
AIRTEL.ret <- Delt(data.AIRTEL$BHARTIARTL.ADJ.CLOSE)
INFY.ret <- Delt(data.INFY$INFY.ADJ.CLOSE)

ret.data <- cbind(REL.ret[-1,], ICICI.ret[-1,],AIRTEL.ret[-1,],
                  INFY.ret[-1,])
names(ret.data) <- paste(c("REL","ICICI","AIRTEL","INFY"))

ret.data[c(1:3, nrow(ret.data))]

data.RELIANCE[nrow(data.RELIANCE),4]
data.ICICIBANK[nrow(data.ICICIBANK),4]
data.AIRTEL[nrow(data.AIRTEL),4]
data.INFY[nrow(data.INFY),4]

last.idx <- c(0.2331,0.8770,0.749,0.1427)*100000
last.idx <- c(30000+30000*0.043,
              30000+30000*0.3084,
              20000+20000*0.0072,
              20000+20000*0.0320)
last.idx
port.val <- sum(last.idx)
port.val

# Calculate Simulated portfolio Returns Applying current portfolio weights to historical security returns

sim.portPnL<-last.idx[1]*ret.data$REL
   last.idx[2]*ret.data$ICICI+
   last.idx[3]*ret.data$AIRTEL +
     last.idx[4]*ret.data$INFY
  
sim.portPnL[c(1:3, nrow(sim.portPnL)),]

names(sim.portPnL) <- paste("Port.PnL")

# Calculate the Appropriate Quantile for the 1 and 5% VaR

VaR01.Historical=quantile(-sim.portPnL$Port.PnL,0.99)
VaR01.Historical<-format(VaR01.Historical,big.mark=',')
VaR01.Historical
  

VaR05.Historical=quantile(-sim.portPnL$Port.PnL,0.95)
VaR05.Historical<-format(VaR05.Historical,big.mark=',')
VaR05.Historical 

# PnL density 
ret.d = density(sim.portPnL$Port.PnL)
ret.d

#plot

plot(ret.d,
     xlab="Profit & Loss",
     ylab="",
    yaxt="n",
     main="Density of Simulated Portfolio P&L Over Three Years And 1% and 
    5% 1-Day Historical Value-at-Risk (VaR)")
abline(v=-quantile(-sim.portPnL$Port.PnL,0.99),col="gray",lty=1)
abline(v=-quantile(-sim.portPnL$Port.PnL,0.95),col="black",lty=2)


x <- seq(min(sim.portPnL$Port.PnL), max(sim.portPnL$Port.PnL), length = 1000)
head(x)

tail(x)

y<-dnorm(x,mean=mean(sim.portPnL$Port.PnL),sd=sd(sim.portPnL$Port.PnL))
head(y)
tail(y)

lines(x,y,type="l",col="black",lwd=1,lty=3)

legend("topright",
        c("Simulated P&L Distribution",
         "Normal Distribution",
         "1% 1-Day VaR","5% 1-Day VaR"),
        col=c("black","black","gray","black"),
        lty=c(1,3,1,2))
