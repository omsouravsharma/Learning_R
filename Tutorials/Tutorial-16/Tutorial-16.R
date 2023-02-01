# Dealing with messy data

# 5.1 Changing Data type

TRUE + 5

v1 <- c(TRUE + 5)
v1
class(v1)

v2 <- c("TRUE" + 1) # ERROR


# Hierarchy of data types in R 
# logical < integer < numeric < character


# Convert functions 

as.logical("FALSE")
as.numeric(TRUE)
as.numeric("2.014")
as.character(2.254)
as.character(TRUE)

# Fail
as.numeric("two")
as.numeric("1+1")


# Working with list. 

my_list <- list(my_number = c(2,6,22,1), 
                my_data = data.frame(a = 1:3, b = 4:6), 
                my_text = "list are the best")
my_list

# Access the elements
my_list$my_data
my_list[[3]]

my_list[[2]][2]

# Splitting vector in lists

# One way
temp_may = airquality$Temp[airquality$Month ==5]
temp_may

# Better way
temps <- split(airquality$Temp, airquality$Month)
temps

names(temps)<- c("May", "June", "July", "August", "September")
temps

# Collapsing list into vector
unlist(temps)

# Working with Number
