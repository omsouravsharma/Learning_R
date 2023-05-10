# Stock Return calculator 

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

str(data.RELIANCE)

df <- cbind(data.frame(index(data.RELIANCE), data.frame(data.RELIANCE)))

fig <- plot_ly(data = df, 
               x = df$index.data.RELIANCE., 
               y = df$RELIANCE.ADJ.CLOSE,
               type = 'scatter',
               mode = 'lines')
fig

# Calculate simple return

df
rel_prices <- df[, "RELIANCE.ADJ.CLOSE", drop = FALSE]

# Denote n the number of time periods:
n <- nrow(rel_prices) 

rel_ret <- ((rel_prices[2:n, 1] - rel_prices[1:(n - 1), 1])/rel_prices[1:(n - 1), 1])

# Notice that rel_ret is not a data frame object
class(rel_ret)

names(rel_ret) <- df[2:n, 1]

head(rel_ret)

#Compute continuously compounded 1-month returns
# Denote n the number of time periods:

n <- nrow(rel_prices)
rel_ret <- ((rel_prices[2:n, 1] - rel_prices[1:(n-1), 1])/rel_prices[1:(n-1), 1])

# Compute continuously compounded 1-month returns
rel_ccret <- log(rel_prices[2:n, 1]) - log(rel_prices[1:(n-1), 1])

# Assign names to the continuously compounded 1-month returns
names(rel_ccret) <- df[2:n, 1]

# Show sbux_ccret
head(rel_ccret)

head(cbind(rel_ret, rel_ccret))

# Plot

# Plot the returns on the same graph
plot(rel_ret, type = "l", col = "blue", lwd = 2, ylab = "Return",
     main = "Monthly Returns on REL")

# Add horizontal line at zero
abline(h = 0)

# Add a legend
legend(x = "bottomright", legend = c("Simple", "CC"), lty = 1, 
       lwd = 2, col = c("blue", "red"))

# Add the continuously compounded returns
lines(rel_ccret, col = "red", lwd = 2)



# Compute gross returns
rel_gret <- 1 + rel_ret

# Compute future values
rel_fv <- cumprod(rel_gret)

# Plot the evolution of the $1 invested in SBUX as a function of time
plot(rel_fv, type = "l", col = "blue", lwd = 2, ylab = "Dollars", 
     main = "FV of $1 invested in SBUX")
