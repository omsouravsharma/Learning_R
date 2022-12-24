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
