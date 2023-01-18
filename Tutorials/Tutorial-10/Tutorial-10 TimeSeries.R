# Exploring Time Series 

# Install 4 packages 
install.packages(c("nlme", "forecast", "fma", "fpp2"), dependencies = TRUE)

#a10 dataset
library(fpp2)
?a10
str(a10)

# LOESS - LOcally Estimated Scatterplot Smooting, is a non-parametric regression method that fits a ploynomial to local areas of the data. 

# GAM - Genralized Additive Model - is a genarlises linear model where the response variable is a linear function of smooth function of the predicators

# convert data to data.frame for ggplot2
a10_df = data.frame(time = time(a10), sales = a10)

ggplot(a10_df, aes(time, sales))+
  geom_point()

#lines
ggplot(a10_df, aes(time, sales))+
  geom_line()

#autoplot automatically draw the appropriate plot for certain type of data. 

library(forecast)
autoplot(a10)

?autoplot

autoplot(a10)+
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


# Longitudinal data

library(fpp2)
?elecdaily
autoplot(elecdaily)
autoplot(elecdaily, facets = TRUE)
str(elecdaily)
autoplot(elecdaily[,1:2], facets = TRUE)+
  geom_smooth()


#Path plot
ggplot(as.data.frame(elecdaily), aes(Temperature, Demand))+
  geom_point()+
  geom_path()

#Add day of the year to the data frame. 

elecdaily2 <- as.data.frame(elecdaily)
elecdaily2$day <- 1:nrow(elecdaily2)

ggplot((elecdaily2), aes(Temperature, Demand, color = day))+
  geom_point(size=0.5)+
  geom_path()

lowest_temp <- which.min(elecdaily2$Temperature)
highest_temp <- which.max(elecdaily2$Temperature)

ggplot(elecdaily2, aes(Temperature, Demand, color = day))+
  geom_point(size=0.5)+
  geom_path()+
  annotate(geom = "text", x = elecdaily2$Temperature[lowest_temp], 
           y = elecdaily2$Demand[lowest_temp]+4, 
           label = elecdaily2$day[lowest_temp])+
  annotate(geom = "text", x = elecdaily2$Temperature[highest_temp], 
           y = elecdaily2$Demand[highest_temp]+4, 
           label = elecdaily2$day[highest_temp])


#Spaghetti plot
library(nlme)
ggplot(Oxboys, aes(age, height, group =Subject))+
  geom_point()+
  geom_line()

# group specifies that their should be one line per subject rather than a single line interpolating all points. 
ggplot(Oxboys, aes(age, height, group = Subject, color = Subject))+
  geom_point()+
  geom_line() #boy are ordered by height. 

#Trend line
ggplot(Oxboys, aes(age, height, color = Subject))+
  geom_point()+
  geom_line(aes(group=Subject))+ #boy are ordered by height.
  geom_smooth(method = "lm", color = "red", se= F)

# Seasonal plot and decomposition 

library(forecast)
ggseasonplot(a10) # useful for visually inspecting seasonal pattern 

ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("Sales ($ million)")+
  ggtitle("Seasonal plot of anti-diabetic drug sales")

# Decompose the series into seasonal components. 

autoplot(stl(a10, s.window = 365))

#stl(a10, s.window = 365)$time.series[, "remainder"]

library(fma)
autoplot(writing)+
  geom_smooth()+
  ylab("Sales (francs")+
  ggtitle("Sales of printing and writing paper")

# Seasonalplot

ggseasonplot(writing, year.labels.left = TRUE, year.labels = TRUE)+
  ylab("Sales (francs")+
  ggtitle("Sales of printing and writing paper")

#stl decomposition 
autoplot(stl(writing, s.window = 365))+
  ggtitle("Sales of printing and writing paper")


# Changepoint detection

install.packages(c("changepoint", "ggfortify"))

library(ffp2)
library(changepoint)
library(ggfortify)

#Plot the timeseries 
autoplot(a10)

#remove the seasonal part 
a10_ns <- a10 - stl(a10, s.window = 365)$time.series[,"seasonal"]
autoplot(a10_ns)

#plot point where there are change in mean.
autoplot(cpt.mean(a10_ns))

# change method
autoplot(cpt.mean(a10_ns, method = "BinSeg"))

# Plot changes where changes are there in variance 
autoplot(cpt.var(a10_ns))

# Plot where there is change in mean or variance 
autoplot(cpt.meanvar(a10_ns))

# Checking on different dataset 
# Plot timeseries
autoplot(elecdaily[, "Demand"])

# plot where changes are in variance 
autoplot(cpt.var(elecdaily[, "Demand"]))

# plot where changes are in variance 
autoplot(cpt.var(elecdaily[, "Temperature"]))

# plot where changes are in variance 
autoplot(cpt.var(elecdaily[, "Demand"]))
#We see that the high-variance period coincides with peaks and large oscillations in
#temperature, which would cause the energy demand to increase and decrease more
#than usual, making the variance greater.


# Making chart interactive 
library(plotly)
myplot <- autoplot(elecdaily[, "Demand"])
ggplotly(myplot)
