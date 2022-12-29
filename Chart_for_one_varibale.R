require("datasets")
data("chickwts")

plot(chickwts$feed)

feeds <- table(chickwts$feed)
feeds

barplot(feeds)
barplot(feeds[order(feeds, decreasing = TRUE)])

par(oma = c(1,1,1,1))
par(mar = c(4,5,2,1)) #Margin

barplot(feeds[order(feeds)], 
        horiz = TRUE, 
        las = 1, 
        col = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"), 
        border = NA, 
        main = "Frequencies of different feeds \nin chickwts dataset",
        xlab = "Number of chicks")


pie(feeds)

pie(feeds[order(feeds, decreasing = TRUE)], 
    init.angle = 90, 
    clockwise = TRUE,
    col = c("beige", "blanchedalmond", "bisque1", "bisque2", "bisque3", "bisque4"),
    main = "Pie chart")


?lynx

data(lynx)

hist(lynx)

h <- hist(lynx, 
          breaks = 11, 
          freq = FALSE, 
          col = "thistle1", 
          main = "Histogram of Annual Candaian Lynx Trappings\n 1821-1934", 
          xlab = "Number of Lynx Trapped")


curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)), 
      col = "thistle4", 
      lwd = 2,
      add = TRUE)

# Boxplot

data("USJudgeRatings")
 
boxplot(USJudgeRatings$RTEN)

boxplot(USJudgeRatings, 
        horizontal = TRUE,
        las = 1, 
        notch = TRUE, 
        ylim = c(1,10),
        col = "Slategray3",
        boxwex = 0.5, 
        whisklty = 1, 
        stapletty = 0,
        outpch = 16, 
        outcol = "Slategray3",
        xlab = "Lawyers Ratings")


# Overlaying Plots
data("swiss")
swiss

str(swiss)

fertility <- swiss$Fertility

# png(filename = "C:/Users/NEXT/Desktop/Learning_R/exported.png", width = 888, 
#     height = 571)

pdf("C:/Users/NEXT/Desktop/Learning_R/exported1.pdf", width = 9, 
         height = 6)

hist(fertility, 
     prob = TRUE, 
     ylim = c(0, 0.04), 
     xlim = c(30,100), 
     breaks = 11, 
     col = "#E5E5E5", 
     border = 0, 
     main = "Fertility for 47 French Speaking\n Swiss Provinces, c1888")

# Plot 2

curve(dnorm(x, mean = mean(fertility), sd = sd(fertility)), 
      col = "red", 
      lwd = 3, 
      add = TRUE)

# Plot 3 

lines(density(fertility), col = "blue")
lines(density(fertility, adjust = 3), col = "darkgreen")


dev.off()

data("iris")
iris

hist(iris$Petal.Length,
     probability = TRUE, 
     breaks = 12,
     col = "#E3E4E2",
     border = 0,
     main = "Iris Dataset"
     )


# Plot 2  Kernel density 
lines(density(iris$Petal.Length), col = "darkred", lwd = 2)

# Plot 3

rug(iris$Petal.Length, col = "darkgrey", lwd = 3)

