# Descriptive Statistics 

# Load ggplot2

library(ggplot2)

View(msleep)

# Important function to analyze dataset.

head(msleep)
tail(msleep)
dim(msleep)
str(msleep)
names(msleep)
?msleep


# To include in environment 
data("msleep")


# Numerical Data

summary(msleep)

# Syntax to access variable in dataframe  data_frame$vector_name

msleep$sleep_total

# mean
mean(msleep$sleep_total)
median(msleep$sleep_total)
max(msleep$sleep_total)
min(msleep$sleep_total)
sd(msleep$sleep_total)
var(msleep$sleep_total)
quantile(msleep$sleep_total)


# How many animals sleep 8 hours a day

sum(msleep$sleep_total >8)


mean(msleep$sleep_rem) # Why NA?

?mean

mean(msleep$sleep_rem, na.rm = TRUE) # Remove the NA

cor(msleep$sleep_total, msleep$sleep_rem)

?cor
cor(msleep$sleep_total, msleep$sleep_rem, use = "complete.obs")


# Categorical Variables 

table(msleep$vore)

# Show proportion

proportions(table(msleep$vore))


# count Cross table

table(msleep$vore, msleep$conservation)


# Proportion per row

proportions(table(msleep$vore,msleep$conservation), margin = 1)
proportions(table(msleep$vore,msleep$conservation), margin = 2)
?proportions




# Dataset Dimaonds
?diamonds

head(diamonds)
tail(diamonds)
dim(diamonds)
str(diamonds)
names(diamonds)

summary(diamonds)



# Plotting Numerical Data

# Scatterplot

# Base R
plot(msleep$sleep_total, msleep$sleep_rem) #Basic

ggplot(msleep, aes(x = sleep_total, 
                   y = sleep_rem)) + geom_point()

plot(msleep$sleep_total, msleep$sleep_rem, pch = 16)
grid()


# Colors, shapes and axis labels
ggplot(msleep, aes(x = sleep_total, 
                   y = sleep_rem)) + 
  geom_point() + 
  xlab("Total sleep time (h)") + 
  ylab("Rem Sleep in hours")


# Added color
ggplot(msleep, aes(x = sleep_total, 
                   y = sleep_rem)) + 
  geom_point(colour = "lightblue") + 
  xlab("Total sleep time (h)") + 
  ylab("Rem Sleep in hours")

colors()


# Categorize by color

ggplot(msleep, aes(x = sleep_total, 
                   y = sleep_rem, colour = vore)) + 
  geom_point() + 
  xlab("Total sleep time (h)") + 
  ylab("Rem Sleep in hours")


# Category by continous variable in color 

ggplot(msleep, aes(x = sleep_total, 
                   y = sleep_rem, colour = sleep_cycle)) + 
  geom_point() + 
  xlab("Total sleep time (h)") + 
  ylab("Rem Sleep in hours")


# Diamond

ggplot(data = diamonds, 
       aes(x = carat, y = price)) +
  geom_point()


# Updated axis:

ggplot(data = diamonds, 
       aes(x = carat, y = price)) +
  geom_point() + 
  xlab("Weights of the diamond (carat)") + 
  ylab("Price (USD")

# Added alpha

ggplot(data = diamonds, 
       aes(x = carat, y = price)) +
  geom_point(alpha=0.25) + 
  xlab("Weights of the diamond (carat)") + 
  ylab("Price (USD")


# size and shape


ggplot(data = diamonds, 
       aes(x = carat, y = price, color = cut)) +
  geom_point(alpha=0.25,  ) + 
  xlab("Weights of the diamond (carat)") + 
  ylab("Price (USD")


# Axis, Limit and scales

ggplot(data = msleep, 
       aes(brainwt, sleep_total, colour = vore)) + 
  geom_point() + 
  xlab("Brain Weight") + 
  ylab("Total sleep time")

# Changing xlim 

ggplot(data = msleep, 
       aes(brainwt, sleep_total, colour = vore)) + 
  geom_point() + 
  xlab("Brain Weight") + 
  ylab("Total sleep time") + 
  xlim(0,1.5)

# Apply log tranformation

ggplot(data = msleep, 
       aes(log(brainwt), sleep_total, colour = vore)) + 
  geom_point() + 
  xlab("Brain Weight") + 
  ylab("Total sleep time") 

# Update scale of x- axis to log 10 

ggplot(data = msleep, 
       aes(brainwt, sleep_total, colour = vore)) + 
  geom_point() + 
  xlab("Brain Weight (logrithmic scale)") + 
  ylab("Total sleep time")  + 
  scale_x_log10()

# Comparing groups 

# To display different between group  use " facetting "

ggplot(msleep, aes(brainwt, sleep_total)) +
  geom_point() +
  xlab("Brain weight (logarithmic scale)") +
  ylab("Total sleep time") +
  scale_x_log10() + 
  facet_wrap(~vore)


# Diamonds Dataset 

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point() + 
  xlab(" Carat") + 
  ylab("Price USD") + 
  facet_wrap( ~ cut)


# Boxplot OR Box or wishkers plot 

# Base R 

boxplot(sleep_total ~ vore, data = msleep)

# ggplot 

ggplot(msleep, aes(vore, sleep_total))+ 
  geom_boxplot()


# Diamonds dataset 

ggplot(data = diamonds, 
       aes(x = price, y = cut)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 1) 

?geom_boxplot


# Histogram 

# Base R
hist(msleep$sleep_total)

# ggplot
ggplot(data = msleep, aes(sleep_total))+ 
  geom_histogram()


# Diamonds dataset

ggplot(diamonds, aes(price)) + 
  geom_histogram(color = "black")+
  facet_wrap(~cut)

?geom_histogram



# Plotting categorical data

# Bar chart - are discrete analogues to histogram

# Base R 

barplot(table(msleep$vore))

# ggplot 

ggplot(msleep, aes(vore)) + 
  geom_bar()

# stacked barchart 

ggplot(msleep, aes(factor(1), fill = vore))+ 
  geom_bar()

ggplot(diamonds, aes(cut)) + 
  geom_bar(fill = c("red", "yellow", "green", "blue", "purple"))

# cut
ggplot(diamonds, aes(cut, fill = cut)) + 
  geom_bar()

# reducing width


ggplot(diamonds, aes(cut, fill = cut)) + 
  geom_bar(width = 0.5)

# clarity

ggplot(diamonds, aes(cut, fill = clarity)) + 
  geom_bar(width = 0.5)

# Position 

ggplot(diamonds, aes(cut, fill = clarity)) + 
  geom_bar(width = 0.5, position = "dodge")

# Horizontal

ggplot(diamonds, aes(cut)) + 
  geom_bar() + 
  coord_flip()

# SAVING A PLOT 

myplot <- ggplot(diamonds, aes(cut)) + 
  geom_bar() + 
  coord_flip()

myplot

myplot + xlab(" New Label")

# ggsave 

ggsave("filename.pdf", myplot, width = 5, height = 5)

dev.off()
