###################################################################################################
### Document
###################################################################################################

# Author:       Felix Gutmann
# Course:       Advanced Computational Methods
# Last update:  14.01.16
# Type:         Problemset 1 - Exercise 1

###################################################################################################
### 0. Praeamble
###################################################################################################

### Clear workspace
rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

### Set working directory
setwd("/home/felix/Dropbox/GSE/DS_GROUP_BOX/Advanced Comp/Homework/Problemset1")

### Initialize auxilliary functions, see handouts

  # Create covariance matrix
  sigmaXY <- function(rho, sdX, sdY) {
    covTerm  <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                       2, 2, byrow = TRUE)
    return(VCmatrix)
  }
  
  # Generate mvn. distributed data 
  genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
  }

###################################################################################################
### Exercise 2
###################################################################################################

# Initialize modified loanData function
loanData <- function(noApproved,  noDenied,  Undecided,
                     muApproved,  muDenied,  muUndecided,
                     sdApproved,  sdDenied,  sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided,
                     seed=1111) 
{
  # Create covariance matrices
  sigmaApproved    <- sigmaXY(rho=rhoApproved,  sdX=sdApproved[1],  sdY=sdApproved[2])
  sigmaDenied      <- sigmaXY(rho=rhoDenied,    sdX=sdDenied[1],    sdY=sdDenied[2])
  sigmaUndecided   <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  # Generate random data
  approved         <- genBVN(noApproved,  muApproved,  sigmaApproved,  seed = seed)
  denied           <- genBVN(noDenied,    muDenied,    sigmaDenied,    seed = seed+1)
  undecided        <- genBVN(Undecided, muUndecided, sigmaUndecided, seed = seed+2)
  # Create basic dataframe
  loanDf           <- as.data.frame(rbind(approved,denied,undecided))
  # Add target variables and labels
  status           <- c(rep("Approved", noApproved), rep("Denied", noDenied),rep("Undecided", Undecided))
  target1          <- c(rep(1, noApproved), rep(0, noDenied),rep(0, Undecided)) 
  target2          <- c(rep(0, noApproved), rep(1, noDenied),rep(0, Undecided))
  target3          <- c(rep(0, noApproved), rep(0, noDenied),rep(1, Undecided))
  # Create final dataframe and prepare output
  loanDf           <- data.frame(loanDf, status, target1,target2,target3)
  colnames(loanDf) <- c("PIratio", "solvency", "status", "target1","target2","target3")
  # Return dataframe with loan data
  return(loanDf)
}
  
# Create dataset
noApproved <- 50
noDenied   <- 50
Undecided  <- 50  

loanDf <- loanData(noApproved, noDenied, Undecided,
                   c(7, 150), c(10, 100), c(13, 250),
                   c(2,  20), c( 2,  30), c( 1,  15),
                   -0.5, 0.3, 0.5)

x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), length.out = noApproved+noDenied+Undecided)

# Run least square discriminant
X <- as.matrix(cbind(intercept=1, loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c('target1','target2','target3')])
W <- solve(t(X)%*%X) %*% t(X) %*% Y

# Compute predictions
predictions <- X %*% W

# classify according to the argmax criterion
result  <- predictions==apply(predictions, 1, max)
labels  <- c("Approved","Denied","Undecided")
predict <- rep(NA,nrow(result))

for(i in 1:nrow(result)){
  pos <- as.numeric(which(result[i,] == TRUE))
  predict[i] <- labels[pos]
}
 
# Prepare final dataframe
loanDf <- cbind(loanDf[,c('PIratio','solvency','status')],predicted=predict)

# Export dataset
write.table(loanDf, file = "predictions.csv",row.names=FALSE, na="",col.names=TRUE, sep=";") 

# Compute discriminant lines
l01 <- ((W[1,2] - W[1,1]) / (W[3,1] - W[3,2])) + ((W[2,2] - W[2,1]) / (W[3,1] - W[3,2])) * x
l02 <- ((W[1,2] - W[1,3]) / (W[3,3] - W[3,2])) + ((W[2,2] - W[2,3]) / (W[3,3] - W[3,2])) * x
l03 <- ((W[1,1] - W[1,3]) / (W[3,3] - W[3,1])) + ((W[2,1] - W[2,3]) / (W[3,3] - W[3,1])) * x 

# Set up boundaries
b01 <- data.frame(PIratio=x, solvency=l01, deny=rep("New Boundary", length(x)))
b02 <- data.frame(PIratio=x, solvency=l02, deny=rep("New Boundary", length(x)))
b03 <- data.frame(PIratio=x, solvency=l03, deny=rep("New Boundary", length(x)))

# Plot result
pdf("discFunction3C.pdf")
  ggplot(data = loanDf, 
         aes(x = solvency, y = PIratio)) + 
    geom_point() +
    xlab("solvency") +
    ylab("PI ratio") +
    theme_bw() +
    geom_line( data = b01) +
    geom_line( data = b02) + 
    geom_line( data = b03) 
dev.off()

###################################################################################################
###### End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE ######
###################################################################################################