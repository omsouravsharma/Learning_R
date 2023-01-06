rm (list = ls())
?warpbreaks

barplot(breaks ~ wool*tension, data = warpbreaks)


data <- tapply(warpbreaks$breaks, list(warpbreaks$wool, warpbreaks$tension), mean)
data

barplot(data, 
        beside = TRUE, 
        col =c("steelblue2", "thistle3"), 
        border = NA, 
        main ="Main Number of Warp Break\n by Tension and Wool",
        xlab = "Tension", 
        ylab = "Mean Number of Break"
        )

legend(locator(1), 
       rownames((data)), 
       fill = c("steelblue2", "thistle3"))


# Scatter Plot

?iris
data("iris")
iris[1:5,]

require(car) # Companion to Applied Regression


sp(Sepal.Width ~ Sepal.Length | Species, data = iris, 
   xlab = "Sepal Width",
   ylab = "Sepal Length", 
   main= "Iris Data"
#   labels = row.names(iris)
   )


pairs(iris[1:4])

# Modified scatter plot
require(RColorBrewer)
display.brewer.pal(3, "Pastel1")

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
  
}

pairs(iris[1:4], 
      panel = panel.smooth, 
      main = "Scatterplot Matrix", 
      diag.panel = panel.hist, 
      pch = 16, 
      col = brewer.pal(3, "Pastel1")[unclass(iris$Species
                                             )])

library(car)
scatterplotMatrix(~Petal.Length + Petal.Width + Sepal.Length + Sepal.Width | Species, 
                  data = iris, 
                  col = brewer.pal(3, "Dark2"), 
                  main = "Scatterplot Matrix")

require("scatterplot3d")
install.packages("scatterplot3d")

scatterplot3d(iris)

s3d <- scatterplot3d(iris[1:3], 
                          pch = 16, 
                          highlight.3d = TRUE, 
                          type = "h", 
                          main = "#D Scatterplot")

plane <- lm(iris$Petal.Length ~ iris$Sepal.Length + iris$Sepal.Width)

s3d$plane3d(plane)

data = read.csv("C:/Users/NEXT/Desktop/Learning_R/Data/SearchData.csv", header = TRUE)
data

pairs(data[c(2:4 , 8:9)], 
      panel = panel.smooth, 
      main = "ScatterPlot Matrix", 
      diag.panel = panel.hist, 
      pch = 16, 
      col = "lightgrey")

scatterplotMatrix(~ nba + nfl + fifa + degree + age, 
                  data =data, 
                  col = brewer.pal(3, "Set3"), 
                  main = "Scatter Plot")
