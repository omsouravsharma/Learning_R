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

