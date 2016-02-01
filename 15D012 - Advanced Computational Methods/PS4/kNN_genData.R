################################################################################
####################   Problemset 4.1.2 - k-NN algorithm    ####################
################################################################################

# Barcelona graduate school of economics

# Programm:		M.S. Data Science
# Author:       (c) Felix Gutmann
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Last update:  04.02.16

# Content:     	This R-file creates uses the function created in the kNN.R file
#               to test predictions. This file contains two main sections
#
#				Section 1 initializes a function to create a multivaria test 
#				data set. By default it creates two classes with bimodal
#				diagonal arranged outcomes, which are not linearly separable 
#			
#				Section 2 performs the function on a created data set and 
#				stores related output

################################################################################
### Praeamble
################################################################################

### Clear workspace
rm(list = ls())

### Set working directory
#setwd("/home/felix/Downloads")

### Load Packages 
if (!require("stats")) 	 install.packages("stats");   library(stats)
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("assert"))  install.packages("assert");  library(assert)

### Initialize auxilliary functions

    # Initialize auxilliary function to generate mvn. data / 
    # Source: Modifiaction from handout 1
    mvsamp <- function(rho, sdX, sdY, n = 1,  mean=c(0,1), seed, sigmaXY=diag(2)) {
    	# Create covariance terms
        covTerm  <- rho * sdX * sdY
        # Create covariance matrix
        VCmatrix <- matrix( c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
        set.seed(seed)
        # Sample data and create output
        draws    <- as.data.frame(rmvnorm(n, mean = mean, sigma = sigmaXY))
        colnames(draws) <- c("V1","V2")
        # Return mvn. draws
        return(draws)
    }

    # Mode function / Source: Stackexchange
    mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    # kNN - classification function
    kNN <- function( features, labels , k = 3, p = 2 ){ 
    	# Check correct input formats
		assert_that(is.data.frame(features) || is.matrix(features))
	  	assert_that(is.vector(labels))
	  	not_empty(features)
	  	not_empty(labels)
        # Count number of points
        N      <- nrow(features)
        # Define arguments for distance method
        method <- c("manhattan","euclidean","maximum")
        # Convert function input to 
        if(p == Inf){
             m <- method[3] } else {
             m <- method[p]
        }
        # Compute distance matrix
        DM  <- as.matrix(dist(features, method = m , diag = FALSE, upper = TRUE) )
        # Compute positions for closest distance
        pos <- lapply(as.data.frame(DM),order)
        # Get predictions
        prediction <- lapply(1:N,
                        function(i){ 
                                    temp <- labels[ pos[[i]] ][2:(k+1)] 
                                    mod  <- mode(temp)
                                    ep   <- length(which(temp==mod)) /length (temp)
                                    return(c(mod,ep))
                                    }
                            )
        # Unlist predictions and store it in a matrix
        con     <- matrix(unlist(prediction), ncol = 2, byrow = TRUE)
        # Prepare output
        output  <- list(predLabels = con[,1] , prob = con[,2] )
        # Return predictions and corresponding probabilities
        return(output)
    }

################################################################################
# Section 1: Initialize function to create test data
################################################################################

genSample   <- function(class1 = 50       , class2 = 50 , 
                        rho1   = 1        , sdX1   = 1  , sdY1     = 1, 
                        mean1  = c(0,0)   , seed1  = 1  , sigmaXY1 = diag(2),
                        rho2   = 1        , sdX2   = 1  , sdY2     = 1, 
                        mean2  = c(15,15) , seed2  = 2  , sigmaXY2 = diag(2),
                        rho3   = 1        , sdX3   = 1  , sdY3     = 1, 
                        mean3  = c(10,10) , seed3  = 3  , sigmaXY3 = diag(2),
                        rho4   = 1        , sdX4   = 1  , sdY4     = 1, 
                        mean4  = c(20,20) , seed4  = 4  , sigmaXY4 = diag(2)) {

 # Generate data according to specified parameters
 samp      <- cbind(mvsamp(rho1, sdX1, sdY1, class1, mean1, seed1, sigmaXY1),y=1)
 diag      <- cbind(mvsamp(rho2, sdX2, sdY2, class1, mean2, seed2, sigmaXY2),y=1)
 up        <- cbind(mvsamp(rho3, sdX3, sdY3, class2, mean3, seed3, sigmaXY3),y=0)
 right     <- cbind(mvsamp(rho4, sdX4, sdY4, class2, mean4, seed4, sigmaXY4),y=0)

 # Set up data set and prepare output
 sim.dat        <- as.data.frame(rbind(samp,diag,up,right))
 names(sim.dat) <- c("x1", "x2", "y")
 # Return final data set
 return(sim.dat)

}

################################################################################
# Section 2: Create test data and save output
################################################################################

# Create data set with default arguments 
data <- genSample()

# Split generated data for function input
X <- data[c("x1","x2")]
Y <- data$y

# Classify data with 5-NN and euclidian distance
classification <- kNN(X,Y,5,2)

# Prepare output
output <- as.data.frame(cbind(X,Y,classification))

# Save prediction data
write.table(output, file = "predictions.csv",row.names=FALSE, col.names=TRUE) 

# Plot data and decision boundaries

################################################################################

################################################################################
