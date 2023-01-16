# Detecting outliers 

library(ggplot2)
ggplot(diamonds, aes(carat, price)) + 
  geom_point()

# Poltly - Interactive version of plot

install.packages("plotly")

# Save plot to a variable 
myplot <- ggplot(diamonds, aes(carat, price)) + 
  geom_point()

library(plotly)
ggplotly(myplot)

# add more text 
myplot <- ggplot(diamonds, aes(carat, price, text= paste("Row:", rownames(diamonds)))) + 
  geom_point()
ggplotly(myplot)


# Check Outlier in length and width of the diamonds

myplot2 <- ggplot(diamonds, aes(x, y))+
  geom_point()
ggplotly(myplot2)

names(diamonds)

# Add more information
myplot2 <- ggplot(diamonds, aes(x, y, text= paste("Row:", rownames(diamonds))))+
  geom_point()
ggplotly(myplot2)

diamonds[24068,]

# Labeling outlier 

# geom_text used to annotate the plot with notes. 

myplot3 <- ggplot(diamonds, aes(carat, price))+
  geom_point()+
  geom_text(aes(label = ifelse(rownames(diamonds)==27416, rownames(diamonds), "")), hjust = 1.1)
ggplotly(myplot3)

# carat >4

myplot4 <- ggplot(diamonds, aes(carat, price))+
  geom_point()+
  geom_text(aes(label = ifelse(carat>4, rownames(diamonds), "")), hjust = 1.1)
ggplotly(myplot4)

## Plot next to 3 carat diamonds with a price below 7500

myplot5 <- ggplot(diamonds, aes(carat, price))+
  geom_point()+
  geom_text(aes(label = ifelse(carat ==3 & price < 7500, rownames(diamonds), "")), hjust = 1.1)
ggplotly(myplot5)


# Missing data 


View(msleep)
is.na(msleep$sleep_rem)

#count NA
colSums(is.na(msleep))
sum(is.na(msleep))
sum(is.na(msleep$sleep_rem))


#ggplot print NA warning 
ggplot(msleep, aes(brainwt, sleep_total))+
  geom_point()+
  scale_x_log10()

ggplot(msleep, aes(bodywt, sleep_total, colour = is.na(brainwt)))+
  geom_point()+
  scale_x_log10()

# Trend in scatterplots

ggplot(msleep, aes(brainwt, sleep_total))+
  geom_point()+
  xlab("Brain Weight (logarithmic scale")+
  ylab("Total sleep time")+
  scale_x_log10()

# To make decreasing trend more visible add new parameters. 
ggplot(msleep, aes(brainwt, sleep_total))+
  geom_point()+
  geom_smooth()+
  xlab("Brain Weight (logarithmic scale")+
  ylab("Total sleep time")+
  scale_x_log10()


# Decrease smoothness
ggplot(msleep, aes(brainwt, sleep_total)) +
  geom_point() +
  geom_smooth(span = 0.25) +
  xlab("Brain weight (logarithmic scale)") +
  ylab("Total sleep time") +
  scale_x_log10()

# Linear Regression
ggplot(msleep, aes(brainwt, sleep_total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("Brain weight (logarithmic scale)") +
  ylab("Total sleep time") +
  scale_x_log10()

ggplot(msleep, aes(brainwt, sleep_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Brain weight (logarithmic scale)") +
  ylab("Total sleep time") +
  scale_x_log10()

ggplot(msleep, aes(brainwt, sleep_total)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  xlab("Brain weight (logarithmic scale)") +
  ylab("Total sleep time") +
  scale_x_log10()

