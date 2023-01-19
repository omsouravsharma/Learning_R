# Using polar coordinates 
library(ggplot2)
ggplot(msleep, aes(sleep_rem, sleep_total, color = vore))+
  geom_point()+
  xlab("REM Sleep (circuar axis)")+
  ylab("Total sleep time (radial axis)")+
  coord_polar()

#Visualizing periodic data
cape_town_weather <- data.frame(Month = 1:12,
                                Temp_C = c(22,23,21,18,16,13,13,13,14,16,18,20), 
                                Rain_mm = c(20,20,30,50,70,90,100,70,50,40,20,20), 
                                Sun_h = c(11,10,9,7,6,6,5,6,7,9,10,11))

ggplot(cape_town_weather, aes(Month, Temp_C))+
  geom_line()

ggplot(cape_town_weather, aes(Month, Temp_C))+
  geom_line()+
  coord_polar()

ggplot(cape_town_weather, aes(Month, Temp_C))+
  geom_line()+
  coord_polar()+
  xlim(0,12)

# Pie chart
ggplot(msleep, aes(factor(1), fill=vore))+
  geom_bar()+
  coord_polar()

# Cartesian bar chart
ggplot(msleep, aes(vore, fill = vore))+
  geom_bar()

# Polar bar chart
ggplot(msleep, aes(vore, fill = vore))+
  geom_bar()+
  coord_polar()


# Visualizing multiple variables 
# Scatter plot matrices

# GGally - for plotting multivariate data

install.packages("GGally")
library(GGally)

ggpairs(airquality)

plot(airquality)

# scatterplot for all the numeric variable. 
ggpairs(diamonds[, which(sapply(diamonds, class)=="numeric")], 
        aes(color = diamonds$cut, alpha = 0.5))+
  ggtitle("Numeric variable plots in diamonds dataset")

# 3D Scatterplot

library(plotly)

plot_ly(airquality, x = ~Ozone, y=~Wind, z= ~Temp, color = ~factor(Month))


# Correlograms 
# efficient to visualize correlated matrix of the data. 

ggcorr(msleep[, c("sleep_total", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt")])
  
ggcorr(diamonds[, which(sapply(diamonds, class)=="numeric")])

ggcorr(diamonds[, which(sapply(diamonds, class)=="numeric")], 
       method = c("pairwise", "spearman"), 
       nbreaks = 5, 
       low = "yellow", 
       high = "black")
# nbreak is used to created categorical class

# adding more information to scatter plot

ggplot(msleep, aes(brainwt, sleep_total, shape=vore))+
  geom_point()+
  scale_x_log10()



ggplot(msleep, aes(brainwt, sleep_total, fill = vore, size = bodywt)) +
  geom_point(alpha = 0.5, colour = "black", shape = 21) +
  xlab("log(Brain weight)") +
  ylab("Sleep total (h)") +
  scale_x_log10() +
  scale_size(range = c(1, 20), trans = "sqrt",
             name = "Square root of\nbody weight") +
  scale_color_discrete(name = "Feeding behaviour")

myplot <- ggplot(msleep, aes(brainwt, sleep_total, fill = vore, size = bodywt)) +
  geom_point(alpha = 0.5, colour = "black", shape = 21) +
  xlab("log(Brain weight)") +
  ylab("Sleep total (h)") +
  scale_x_log10() +
  scale_size(range = c(1, 20), trans = "sqrt",
             name = "Square root of\nbody weight") +
  scale_color_discrete(name = "Feeding behaviour")
ggplotly(myplot)


# Overplotting 

ggplot(diamonds, aes(table, depth))+geom_point() #example of overplotting 

ggplot(diamonds, aes(table, depth))+geom_point(size=0.1)
ggplot(diamonds, aes(table, depth))+geom_point(alpha = 0.2)

#bin2d
ggplot(diamonds, aes(table, depth))+
  geom_bin2d(bins = 50)

#hex

ggplot(diamonds, aes(table, depth))+
  geom_hex(bins = 50)

#sta_density_2d 
ggplot(diamonds, aes(table, depth))+
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", color = "white")

#tile plot 

ggplot(diamonds, aes(table, depth, z = price))+
  geom_tile(binwidth = 1, stat = "summary_2d", fun = mean)+
  ggtitle("Mean price for diamonds with different depth and tables")

# Categorical Data
# geom_count 

ggplot(diamonds, aes(color, cut))+
  geom_count()

 # calculate count 
diamonds2<- aggregate(carat ~ cut + color, data = diamonds, FUN = length)
diamonds2

names(diamonds2)[3] <- "Count"
ggplot(diamonds2, aes(color, cut, fill= Count))+
  geom_tile()

ggplot(diamonds2, aes(color, cut, color= Count, size = Count))+
  geom_count()
