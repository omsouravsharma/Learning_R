# 1. R Installation
# Link - https://cran.r-project.org/mirrors.html
# Download R 
# Download R Studio








# 2. R Studio overview 

# Environment Panel
# File, Plot and Help Panel
# Console Panel 
# Script Panel











# 3. Running R code
# Rscript 

1+1
2*3
1+2*3
(1+2)*3-5

# Creating New File 
# Running Script option
# Saving file









# 4. Variables and Function 

x<-4

x+ 1
x +x 
X + x # Error Case Sensitive 


x <- 1+3 *3 # Computation

x< - 1 + 3 +3 # Why False?

2+ 2 -> y # Right assignment

# Note:- Variable name should be informative

a = 1000
income = 1000

# Naming Conventions
# 1. snake_case
# 2. CamelCase or camelCase
# 3. period.case
# 4. concatenatedwordscase
# 5. SCREAMING_SNAKE_CASE

#################################
# Comments                      #
#################################

# Vectors and data frames

# List is called vector 

age <- c(28,48,47,71,22,80,48,30,31) # c stand for combine 

age*12

purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)
purchase
# Combine vector into table 

bookstore <- data.frame(age, purchase)



# Why this [1] appears?

distances <- c(687, 5076, 7270, 967, 6364, 1683, 9394, 5712, 5206,
               4317, 9411, 5625, 9725, 4977, 2730, 5648, 3818, 8241,
               5547, 1637, 4428, 8584, 2962, 5729, 5325, 4370, 5989,
               9030, 5532, 9623)
distances

# indices of the elements 

x <-4 
x #Everything in R is vector 

# Functions

# In built functions
mean(age)

# Compute the correlation between two variables

cor(age, purchase)

cor(age, purchase, method = "spearman")

# How to get help
?cor

age
purchase












# 5. Mathematical Operations

# Basic Arithmetic operators

abs(0.2)
sqrt(3)
log(2)
2^3
exp(2)
sin(2)
sum(2,4,6,22)
prod(3,5)
pi
10%%3
factorial(5)
choose(16, 4)
?choose
sqrt(-1)
1/0
0/0




# Command to remove the Global Environment 
rm(list = ls())
