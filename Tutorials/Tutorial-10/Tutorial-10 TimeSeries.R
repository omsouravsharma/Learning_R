# Exploring Time Series 

# Install 4 packages 
install.packages(c("nlme", "forecast", "fma", "fpp2"), dependencies = TRUE)

#a10 dataset
library(fpp2)
?a10
str(a10)

#LOESS - LOcally Estimated Scatterplot Smooting, is a non-parametric regression method that fits a ploynomial to local areas of the data. 

# GAM - Genralized Additive Model - is a genarlises linear odel where the response variable is a linear function of smooth function of the predicators

# convert data to data.frame for ggplot2
a10_df = data.frame(time = time(a10), sales = a10)

ggplot(a10_df, aes(time, sales))+
  geom_point()

#lines
ggplot(a10_df, aes(time, sales))+
  geom_line()

#autoplot automatically draw the appropriate plot for certain type of data. 

library(forcast)
autoplot(a10)

?autoplot

autoplot(a10)+
  geom_smooth(color = "red")+
  xlab("Year")+
  ylab("Sales ($ million)")+
  labs(title = "Autoplot")

?ggtitle

# Annotations and reference lines
autoplot(gold) # Gold dataset

spike_date <- which.max(gold)
spike_date
autoplot(gold)+ 
  annotate(geom = "point" , x = spike_date, y = gold[spike_date], size = 6, shape=21, 
           color = "red", 
           fill= "transparent")+
  annotate(geom = "text", x = spike_date, y = gold[spike_date]+10,
           label = "Incorrect Value",
           color="red")
?annotate

# Remove incorrect value 
gold[-770]
gold[spike_date]<-NA
autoplot(gold)

#add reference line
autoplot(gold) + 
  geom_hline(yintercept = 400, color ="red")
