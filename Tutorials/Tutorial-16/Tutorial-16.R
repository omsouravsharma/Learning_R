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

# Rounding 

a <- c(2.554, 2.523, 6.32,4.24 )
round(a, 2)

signif(a,3)

ceiling(a)

floor(a)

trunc(a)


# Sum and mean in data frame 

bookstore <- data.frame(purchase1 = c(20,29,2,12,22,160, 34,34,29), 
                        purchase2 = c(14,67,9,20,20,81,19,55,8), 
                        purchase3 = c(4,62,11,18,33,57,24,49,29))
bookstore
colSums(bookstore)
rowSums(bookstore)

colMeans(bookstore)
rowMeans(bookstore)


# Summaries of series of numbers 

library(fpp2)
a10
a10[7:18] # sales of 1992

cumsum(a10[7:18])

cummax(a10[7:18])
cummin(a10[7:18])

# Lowest monthly sales 
# plot total sales up to and include each month

plot(1:12, cumsum(a10[7:18]), 
     xlab = "Month", 
     ylab = "Totoal Sales", 
     type = "b")

# format scientific notoation 

format(7000000, scientific = FALSE)
format(7000000, scientific = TRUE)

1/3

1.5-0.2 == 1.3
1.1-0.2 == 0.9

sprintf("%.30f", 1.1-0.2)
sprintf("%.30f", 1.5-0.2)
sprintf("%.30f", 1.3)


all.equal(1.1-0.2, 0.9) # near equality

all.equal(1,2)
isTRUE(all.equal(1,2))
?all.equal


# Working with factor 

smoke <- c("Never", "Never","Heavy","Never" ,"Occassionally", 
           "Never", "Never", "Regularly", "Regularly", "No")

table(smoke)

# Creating factor
smoke2 <- factor(smoke)
smoke2

levels(smoke2)

smoke2 <- factor(smoke, levels = c("Never", "Occassionally", "Regularly", "Heavy"), ordered = T)
smoke2

levels(smoke2)
table(smoke2)

# To check the NA

smoke[which(is.na(smoke2))]

# Changing the factor level 

smoke2 <- addNA(smoke2)
smoke2

levels(smoke2)[5]<- "Invalid answer"

# combining levels 

levels(smoke2)[2:4] <- "Yes"
levels(smoke2)
smoke2

# working with strings 

text1 <- "An example of string. Isn't this is great"
text1
text2 <- 'An example of so called "string"'
text2

text3 <- "Text..\n\tWith indented text on a new line!"
text3
cat(text3)

# Concatenating string

first <- "This is first"
second <- "and this is end"

cat(first, second)
cat(first, second, sep = "\n")

# Upper and lower case

my_string <- "SOMETIMES I SCREAM (and sometime I whisper)"
toupper(my_string)
tolower(my_string)

# Working with dates
