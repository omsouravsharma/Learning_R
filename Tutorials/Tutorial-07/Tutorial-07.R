# Customising ggplot2 plot

library(ggplot2)

ggplot(msleep, aes(brainwt, sleep_total)) + 
  geom_point() + 
  xlab("Brain weight (logrithmic scale)")  + 
  ylab("Total Sleep time") + 
  scale_x_log10() + 
  facet_wrap(~vore)

# Modifying with themse
p <- ggplot(msleep, aes(brainwt, sleep_total, color = vore)) + 
  geom_point() + 
  xlab("Brain weight (logrithmic scale)")  + 
  ylab("Total Sleep time") + 
  scale_x_log10() + 
  facet_wrap(~vore)

# Create plot with different themes
p+ theme_gray()
p+ theme_bw()
p+ theme_linedraw()
p+ theme_light()
p+ theme_dark()
p+ theme_minimal()
p+ theme_classic()


# Theme package
install.packages("ggthemes")
library(ggthemes)

p + theme_tufte()
p + theme_wsj() # Wall street journel
p + theme_solarized() + scale_color_solarized()


install.packages("hrbrthemes")
library(hrbrthemes)

p + theme_ipsum()
p + theme_ft_rc()
p + theme_modern_rc()


# Color Palettes

# Three Types of color palette are available 
# 1. Sequential palettes 
# 2. Diverging palettes 
# 3. Qualitative palettes

# sequential 
p + scale_color_brewer(palette = "OrRd")

# Diverging palette
p + scale_color_brewer(palette = "RdBu")

# Qualitative
p + scale_color_brewer(palette = "Set1")

# Additional theme settings

p + theme(legend.position = "none")
p + theme(legend.position = "bottom")
p + theme(legend.position =  c(0, 1)) # relative coordinates 


p + theme(panel.grid.major = element_line(colour = "black"),
          panel.grid.minor = element_line(colour = "purple",
                                          linetype = "dotted"),
          panel.background = element_rect(colour = "red", size = 2),
          plot.background = element_rect(fill = "yellow"),
          axis.text = element_text(family = "mono", colour = "blue"),
          axis.title = element_text(family = "serif", size = 14))
