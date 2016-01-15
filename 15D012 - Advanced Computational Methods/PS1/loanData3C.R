###################################################################################################

###################################################################################################

# Author:       Felix Gutmann
# Course:       Advanced Computational Methods
# Last update:  14.01.16
# Type:         Problemset 1 - Exercise 2

###################################################################################################
### Praeamble
###################################################################################################

### Clear workspace
#rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

### Set working directory

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

### Step 1: Create data generating function

# Initialize modified loanData function
loanData <- function(noApproved,  noDenied,  Undecided,
                     muApproved,  muDenied,  muUndecided,
                     sdApproved,  sdDenied,  sdUndecided,
                     rhoApproved, rhoDenied, rhoUndecided,
                     seed=1111) 
{
  # Create covariance matrices
  sigmaApproved    <- sigmaXY( rho = rhoApproved,  sdX = sdApproved[1],  sdY = sdApproved[2]  )
  sigmaDenied      <- sigmaXY( rho = rhoDenied,    sdX = sdDenied[1],    sdY = sdDenied[2]    )
  sigmaUndecided   <- sigmaXY( rho = rhoUndecided, sdX = sdUndecided[1], sdY = sdUndecided[2] )
  # Generate random data
  approved         <- genBVN( noApproved, muApproved,  sigmaApproved,  seed = seed   )
  denied           <- genBVN( noDenied,   muDenied,    sigmaDenied,    seed = seed+1 )
  undecided        <- genBVN( Undecided,  muUndecided, sigmaUndecided, seed = seed+2 )
  # Create basic dataframe
  loanDf           <- as.data.frame( rbind(approved,denied,undecided) )
  # Add target variables and labels
  status           <- c( rep("Approved", noApproved), rep("Denied", noDenied),rep("Undecided", Undecided) )
  target1          <- c( rep(1, noApproved), rep(0, noDenied),rep(0, Undecided) ) 
  target2          <- c( rep(0, noApproved), rep(1, noDenied),rep(0, Undecided) )
  target3          <- c( rep(0, noApproved), rep(0, noDenied),rep(1, Undecided) )
  # Create final dataframe and prepare output
  loanDf           <- data.frame( loanDf, status, target1,target2,target3 )
  colnames(loanDf) <- c("PIratio", "solvency", "status", "target1","target2","target3")
  # Return dataframe with loan data
  return(loanDf)
}

### Step 2: Create, export and predict related data

# Create dataset
noApproved <- 50
noDenied   <- 50
Undecided  <- 50  
noObs      <- noApproved + noDenied + Undecided

loanDf <- loanData(noApproved, noDenied, Undecided,
                   c(7, 150), c(10, 100), c(13, 250),
                   c(2,  20), c( 2,  30), c( 1,  15),
                  -0.5, 0.3, 0.5)

x <- seq(min(loanDf["PIratio"]), max(loanDf["PIratio"]), length.out = noObs)

# Run least square discriminant
X <- as.matrix(cbind(intercept=1, loanDf[,c("PIratio", "solvency")]))
Y <- as.matrix(loanDf[,c('target1','target2','target3')])
W <- solve(t(X)%*%X) %*% t(X) %*% Y

# Compute predicted values
predictions <- X %*% W

# classify data and create corresponding labels
result  <- predictions==apply(predictions, 1, max)
labels  <- c("Approved","Denied","Undecided")
predict <- rep(NA,nrow(result))

for(i in 1:nrow(result)){
  pos <- as.numeric(which(result[i,] == TRUE))
  predict[i] <- labels[pos]
}

# Prepare final dataframe - collect relevant columns
colnames(predictions) <- c("Approved","Denied","Undecided")
loanDf <- cbind(loanDf[,c('PIratio','solvency','status')],predictions,"Predicted label"=predict)

# Export dataset
write.table(loanDf, file = "predictions.csv",row.names=FALSE, na="",col.names=TRUE, sep=";") 

### Step 3: Plot data with decision boundaries

# Compute discriminant lines (see Bishop:)
l01 <- ((W[1,2] - W[1,1]) / (W[3,1] - W[3,2])) + ((W[2,2] - W[2,1]) / (W[3,1] - W[3,2])) * x
l02 <- ((W[1,2] - W[1,3]) / (W[3,3] - W[3,2])) + ((W[2,2] - W[2,3]) / (W[3,3] - W[3,2])) * x
l03 <- ((W[1,1] - W[1,3]) / (W[3,3] - W[3,1])) + ((W[2,1] - W[2,3]) / (W[3,3] - W[3,1])) * x 

# Set up data frame with line data
b01 <- data.frame(PIratio=x, solvency=l01, status=rep("Boundary 1", length(x)))
b02 <- data.frame(PIratio=x, solvency=l02, status=rep("Boundary 2", length(x)))
b03 <- data.frame(PIratio=x, solvency=l03, status=rep("Boundary 3", length(x)))

# Find cut position
X01   <- round(cbind(1,x,l01) %*% W,8)
pos01 <- X01[,2] > X01[,3]
if(pos01[1] == TRUE ){ rangeb01 <- 1:sum(pos01) } else { rangeb01 <- (noObs-sum(pos01)):noObs }

X02   <- round(cbind(1,x,l02) %*% W,8)
pos02 <- X02[,2] > X02[,1]
if(pos02[1] == TRUE ){ rangeb02 <- 1:sum(pos02) } else { rangeb02 <- (noObs-sum(pos02)):noObs }

X03   <- round(cbind(1,x,l03) %*% W,8)
pos03 <- X03[,1] > X03[,2]
if(pos03[1] == TRUE ){ rangeb03 <- 1:sum(pos03) } else { rangeb03 <- (noObs-sum(pos03)):noObs }

# Plot result
plot01 <- ggplot(data = loanDf, aes( x = solvency, y = PIratio, colour = status, fill = status ) ) + 
            geom_point() +
            xlab("Solvency") +
            ylab("PI ratio") +
            theme_bw() +
            geom_line( data = b01[rangeb01,]) +
            geom_line( data = b02[rangeb02,]) + 
            geom_line( data = b03[rangeb03,]) +
            scale_color_manual("status", 
                                values = c("Boundary 1" = "black" , "Boundary 2" = "blue" , "Boundary 3" = "orange",
                                            "Approved"  =  "green" , "Denied"     = "red" , "Undecided"  = "grey"
                                          )
                              )
# Save plot
ggsave( plot01,file="discFunction3C.pdf" )

###################################################################################################
###### End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE ######
###################################################################################################