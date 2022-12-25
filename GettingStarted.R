library() # To view all
# Install package

install.packages("ggplot2")


# Loading package. 
require("ggplot2")

# Vingettes
vignette(package='grid')
browseVignettes(package='grid')

# Update Package. 

update.packages()

# Help
?update.packages
?detach

# Remove Package

remove.package("psytabs")

# Built-in dataset

?datasets

library(help='datasets')

?airmiles

data("airmiles")

require("graphics")
plot(airmiles, main = "airmiles data", xlab = "Passanger data", col=4)

# Entering data manually

x1 = 1:100

x2 = 10

x3 <- seq(10)

x4 <- seq(30,0, by = -3)

x5 <- c(2,4,6,3,2,5,2) # Concatenate 

x6 <- scan()


# Importing data into R 
data = read.table('C:/Users/NEXT/Desktop/Learning_R/Data/text.txt', header = TRUE, sep =",")
str(data)

rm(list = ls())

# Converting tabular data to row. 

?UCBAdmissions
str(UCBAdmissions)
UCBAdmissions

plot(UCBAdmissions)

plot(margin.table(UCBAdmissions,1))
margin.table(UCBAdmissions, 2)
margin.table(UCBAdmissions, 3)
margin.table(UCBAdmissions)

admit.dept <-margin.table(UCBAdmissions, 3)
str(admit.dept)
barplot(admit.dept)

admit.dept
round(prop.table(admit.dept),2) # Proposition

round(prop.table(admit.dept),2)*100 # Proposition


admit1 <- as.data.frame.table(UCBAdmissions)

admit2 <- lapply(admit1, function(x)rep(x, admit1$Freq))

admit3 <- as.data.frame(admit2)

admit4 <- admit3[, -4]


x <- c(12,4,21,17,13,8)
barplot(x)

colors()
barplot(x, col = "slategray3")
barplot(x, col = colors()[234])

# RGB Trippet
col2rgb("navyblue")
barplot(x, col = rgb(.54,.0,.0))
barplot(x, col = rgb(145,111,222, max = 255))

barplot(x, col="#FFEBCD")

# PALETTES

palette()
barplot(x, col =1:6)
barplot(x, col =rainbow(6))
barplot(x, col =heat.colors(6))
barplot(x, col =terrain.colors(6))
barplot(x, col =topo.colors(6))
barplot(x, col =cm.colors(6))

palette("default")
