# Exploratory factor Analysis 

# Factor Analysis 
install.packages(c("psych", "GPArotation"))
?attitude

attitude

library(psych)

# Fit attitude models

attitude_fa <- fa(attitude, nfactors = 2, rotate = "oblimin", fm= "ml")
attitude_fa # result

# plot result 

fa.diagram(attitude_fa, simple = FALSE)

attitude_fa <- fa(attitude, nfactors = 2, rotate = "varimax", fm = "ml") 
fa.diagram(attitude_fa, simple = FALSE)

attitude_fa <- fa(attitude, nfactors = 2, rotate = "oblimin", fm = "miners") 
fa.diagram(attitude_fa, simple = FALSE)

scree(attitude, pc = FALSE)

fa.parallel(attitude, fm = "ml", fa = "fa")


# Latent class analysis

