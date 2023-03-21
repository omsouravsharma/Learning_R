# Two Asset (Matrix Algebra)
# Create Vector of Weights

WGT.2asset <- c(0.25,0.75)
WGT.2asset

WGT.2asset <- matrix(WGT.2asset,1)
WGT.2asset

# Create a Transposed Vector of Weights
tWGT.2asset<- t(WGT.2asset)
tWGT.2asset

#Construct Variance - Covariance Matrix