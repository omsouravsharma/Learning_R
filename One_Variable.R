# Calculating frequencies

groups <- c(rep("blue", 3990),
           rep("red", 4140),
           rep("orange", 1890),
           rep("green", 3770),
          rep("purple", 855)
           )

# Create Frequency Tables

group.t1 <- table(groups)
group.t1

group.t2 <- sort(group.t1, decreasing = TRUE)
group.t2

prop.table(group.t2)
group.t3 <- round(prop.table(group.t2),2)*100

barplot(group.t3, 
        horiz = TRUE,
        col = brewer.pal(5, "Set2"))

?RColorBrewer

# Calculating Descriptives. 

