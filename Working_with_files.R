?mtcars

mtcars

mean(mtcars$qsec[mtcars$cyl == 8])

median(mtcars$hp)


mean(mtcars$mpg[mtcars$hp > median(mtcars$hp)])

cyl.8 <- mtcars[mtcars$cyl == 8, ]
cyl.8


mtcars[mtcars$cyl ==8 & mtcars$carb >=4, ]

# Analyzing by subgroups 

?iris

iris

aggregate(iris$Petal.Width ~ iris$Species, FUN = mean)

aggregate(cbind(iris$Petal.Width, iris$Petal.Length) ~ iris$Species, FUN = mean)


# Merging files 

?longley
data("longley")

a1 <- longley[1:14, 1:6]
a1
a2 <- longley[1:14, 6:7]
a2

b <- longley[15:16,]
b

write.table(a1, "C:/Users/NEXT/Desktop/Learning_R/Data/a1.txt", sep = "\t")
write.table(a2, "C:/Users/NEXT/Desktop/Learning_R/Data/a2.txt", sep = "\t")
write.table(b, "C:/Users/NEXT/Desktop/Learning_R/Data/b.txt", sep = "\t")

# Import data 

a1t <- read.table("C:/Users/NEXT/Desktop/Learning_R/Data/a1.txt", sep = "\t")
a2t <- read.table("C:/Users/NEXT/Desktop/Learning_R/Data/a2.txt", sep = "\t")
b <- read.table("C:/Users/NEXT/Desktop/Learning_R/Data/ab.txt", sep = "\t")

a.1.2<-merge(a1t, a2t, by = "Year")

al.data = rbind(a.1.2, b) <-NULL
al.data

data('ToothGrowth')

ToothGrowth
aggregate(ToothGrowth$len ~ ToothGrowth$supp, FUN = mean)
aggregate(ToothGrowth$len ~ ToothGrowth$supp, FUN = median)

