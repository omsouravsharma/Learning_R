# Constructing Portfoilio Return (Long Way)
# Weighted average return of the individual securities. 

# w1 ∗ r1 + w2 ∗ r2

# 1. Find first and last Adjusted closing price.
conflictRules('dplyr', exclude = 'lag')
library(xts)
library(quantmod)

data.ret <-data.frame()
ICICI <- c(447.200012,842.650024)
INFY <- c(642.349976, 1471.550049)
REL <- c(1094.921265,2322.699951)
TATAC <- c(324.25,704.400024)
Dates <- as.Date(c('13-03-2020','10-03-2023'), format = "%d-%m-%Y")
data.ret <- data.frame(ICICI, INFY,REL,TATAC)

data.ret <- as.xts(data.ret, order.by = data.ret$Dates)
#data.ret <- data.frame(data.ret)      
data.ret <- data.ret[,2:5]
data.ret
class(data.ret)
# Calcuate the returns

rets <- lapply(data.ret, Delt)
rets

rets <- data.frame(rets)
rets

# Covert to %
rets <-rets[2,]*100
rets
names(rets)<- paste(c("ICICI", "INFY","RELS","TATAC"))
rets

# Calculate the weighted of portfolio 
