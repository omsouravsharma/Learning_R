# Exploratory factor Analysis 

# Factor Analysis 
install.packages(c("psych", "GPArotation"))
?attitude

attitude

library(psych)

# Fit attitude models

attitude_fa <- fa(attitude, nfactors = 2, rotate = "oblimin", fm= "ml")
attitude_fa
