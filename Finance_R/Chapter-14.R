# Calculating the Variance and SD of INFY

library(xts)
library(quantmod)
#INFY

data.INFY <- read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/stock/INFY.csv", 
                      header = TRUE)
data.INFY$Date <-as.Date(data.INFY$Date, format = "%Y-%m-%d")
data.INFY <- data.INFY[order(data.INFY$Date), ]
data.INFY <- as.xts(data.INFY[, 2:7], order.by = data.INFY$Date)
head(data.INFY)

# Calculate return

INFY.ret <- data.INFY[,4]
INFY.ret$ret <- Delt(INFY.ret$Close)
head(INFY.ret)

INFY.ret<- INFY.ret[-1,2]
head(INFY.ret)
INFY.Full.VAR <- var(INFY.ret$ret)

INFY.Full.SD <- sd(INFY.ret$ret)

#cal 2018

infy.2018 <- subset(INFY.ret, index(INFY.ret)>= "2018-01-01" &
                      index(INFY.ret)<="2018-12-31")
head(infy.2018)

infy.var.2018 <- var(infy.2018)
infy.var.2018
infy.sd.2018 <- sd(infy.2018)
infy.sd.2018
#cal 2019

infy.2019 <- subset(INFY.ret, index(INFY.ret)>= "2019-01-01" &
                      index(INFY.ret)<="2019-12-31")

infy.var.2019 <- var(infy.2019)
infy.sd.2019 <- sd(infy.2019)

#cal 2020
infy.2020 <- subset(INFY.ret, index(INFY.ret)>= "2020-01-01" &
                      index(INFY.ret)<="2020-12-31")

infy.var.2020 <- var(infy.2020)
infy.sd.2020 <- sd(infy.2020)

#cal 2021
infy.2021 <- subset(INFY.ret, index(INFY.ret)>= "2021-01-01" &
                      index(INFY.ret)<="2021-12-31")

infy.var.2021 <- var(infy.2021)
infy.sd.2021 <- sd(infy.2021)

#cal 2022
infy.2022 <- subset(INFY.ret, index(INFY.ret)>= "2022-01-01" &
                      index(INFY.ret)<="2022-12-31")

infy.var.2022 <- var(infy.2022)
infy.sd.2022 <- sd(infy.2022)

#cal 2023
infy.2023 <- subset(INFY.ret, index(INFY.ret)>= "2023-01-01" &
                      index(INFY.ret)<="2023-12-31")

infy.var.2023 <- var(infy.2023)
infy.sd.2023 <- sd(infy.2023)

#calcuate the Avergae return #mean

mean.ret.full <- mean(INFY.ret)
mean.ret.2018 <- mean(infy.2018)
mean.ret.2019 <- mean(infy.2019)
mean.ret.2020 <- mean(infy.2020)
mean.ret.2021 <- mean(infy.2021)
mean.ret.2022 <- mean(infy.2022)
mean.ret.2023 <- mean(infy.2023)

#combine data


INFY.Risk <- rbind(
  cbind(INFY.Full.VAR, infy.var.2018,infy.var.2019,
        infy.var.2020,infy.var.2021,infy.var.2022,infy.var.2023),
  cbind(INFY.Full.SD, infy.sd.2018,infy.sd.2019,
       infy.sd.2020,infy.sd.2021,infy.sd.2022,infy.sd.2023),
  cbind(mean.ret.full, mean.ret.2018,mean.ret.2019,
        mean.ret.2020,mean.ret.2021,mean.ret.2022,mean.ret.2023))

INFY.Risk

options(digits=3)
rownames(INFY.Risk)<-c("Variance","Std Dev","Mean")
colnames(INFY.Risk)<-c("2018-2023","2018","2019","2020","2021","2022","2023")
INFY.Risk
options(digits=7)
