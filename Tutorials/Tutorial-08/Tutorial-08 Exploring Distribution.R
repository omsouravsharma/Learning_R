# Exploring Distribution 

# Density plot and frequency polygons 

library(ggplot2)

ggplot(diamonds, aes(x = carat))  +
  geom_histogram(color= "black")

# frequency polygons 

ggplot(diamonds, aes(carat))+ 
  geom_freqpoly()

# This can be used to compare groups 

ggplot(diamonds, aes(carat, color = cut))+ 
  geom_freqpoly()

?geom_freqpoly

# Density plots
ggplot(diamonds, aes(carat, color = cut))+ 
  geom_density()

# Improved smoothness
# Filled curve
# Make filled color transparent

ggplot(diamonds, aes(carat, color = cut, fill = cut))+ 
  geom_density(bw = 0.2, alpha = 0.2)

install.packages("ggridges")
library(ggridges)

ggplot(diamonds, aes(carat, cut, fill = cut))+ 
  geom_density_ridges()


ggplot(diamonds, aes(carat)) + geom_histogram(binwidth = 0.01)+
  xlim(0, 3)
# Conclusion - Diamonds with carat values that are multiples of 0.25 are more common than others. 

ggplot(diamonds, aes(carat)) +
       geom_point()


# Extract information from a histogram with bin width 0.01 = 481
carat_br <- hist(diamonds$carat, breaks = 481, right = FALSE, plot = FALSE)
carat_br
# Create category for each bin 
diamonds$carat_cat <- cut(diamonds$carat, 481, right = FALSE)
diamonds

# Compute the mean for each bin

means <- aggregate(price ~ carat_cat, data = diamonds, FUN= mean)
plot(carat_br$mid, means$price) # ERROR Expected

# Fix 

means <- aggregate(price ~ carat_cat, data = diamonds, FUN= mean)
id<- match(means$carat_cat, levels(diamonds$carat_cat))

#Base
plot(carat_br$mids[id], means$price, cex = 0.5)
# Add vertical lines at multiples of 0.25:
abline(v = c(0.5, 0.75, 1,1.25, 1.5))

# ggplot

d2 <- data.frame(bin = carat_br$mid[id],mean = means$price)
ggplot(d2, aes(bin, mean)) +
  geom_point() +
  geom_vline(xintercept =
               c(0.5, 0.75, 1,1.25, 1.5))

# Violin plot as an alternative to boxplot 

ggplot(diamonds, aes(cut, price)) + 
  geom_boxplot()

ggplot(diamonds, aes(cut, price)) + 
  geom_violin()

# add quantiles 
ggplot(diamonds, aes(cut, price)) + 
  geom_violin( draw_quantiles = c(0.25, 0.5, 0.75))

# More updates 

ggplot(diamonds, aes(cut, price, fill = cut)) + 
  geom_violin( draw_quantiles = c(0.25, 0.5, 0.75))

# Combining violin and boxplot 

ggplot(diamonds, aes(cut, price)) + 
  geom_violin(aes(fill= cut), width = 1.25, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(width = 0.1, alpha = 0.5)+
  theme(legend.position = "none")+
  coord_flip()


# Combine multiple plots into a single graphics
install.packages("patchwork")

plot1 <- ggplot(diamonds, aes(cut, carat, fill= cut))+
  geom_violin()+
  theme(legend.position = "none")

plot2 <- ggplot(diamonds, aes(cut, price, fill = cut))+
  geom_violin()+
  theme(legend.position = "none")
  

library(patchwork)

plot1 + plot2

plot3 <- ggplot(diamonds, aes(cut, depth, fill= cut))+
  geom_violin()+
  theme(legend.position = "none")

plot4 <- ggplot(diamonds, aes(carat, fill = cut))+
  geom_density(alpha = 0.5)+
  theme(legend.position = c(0.9,0.6))

(plot1|plot2|plot3)/plot4
