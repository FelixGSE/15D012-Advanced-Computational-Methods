################################################################################
####################   Problemset 4.1.1 - k-NN algorithm    ####################
################################################################################

# Author:       (c) Felix Gutmann
# Course:       15D012 - Advanced Computational Methods
# Last update:  04.02.16

# Content:     	This R-file creates a function performing the k-nearest-
#				neighbhor algorithm on a given input data set (Section 1). 
#				The function returns a list with predicted labels and 
#				corresponding probabilities.
#
#				Function arguments: 
#
#					1. features:		Matrix or data frame with features
#					2. labels:			A vector with corresponding labels
#					3. k:				Parameter for the number of NN
#					4. p:				Distance meassure (1=L1,2=L2,Inf=max)
#
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

### Initialize auxilliary functions

    ### Initialize auxilliary function to generate mvn. data / 
    ### Source: Modifiaction from handout 1
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

################################################################################
# Section 1: Initialize k-Nearest-Neighbhor function
################################################################################

kNN <- function( features, labels , k = 3, p = 2 ){ 
	# Count number of points
	N 	   <- nrow(features)
	# Define arguments for distance method
	method <- c("manhattan","euclidean","maximum")
	# Convert function input to 
	if(p == Inf){
		 m <- method[3] } else {
		 m <- method[p]
	}
	# Compute distance matrix
	DM 	<- as.matrix(dist(features, method = m , diag = FALSE, upper = TRUE) )
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
	con 	<- matrix(unlist(prediction), ncol = 2, byrow = TRUE)
	# Prepare output
	output 	<- list(predLabels = con[,1] , prob = con[,2] )
	# Return predictions and corresponding probabilities
	return(output)
}

################################################################################

################################################################################