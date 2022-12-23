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
