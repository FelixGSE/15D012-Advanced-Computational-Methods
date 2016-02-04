################################################################################
####################   Problemset 4.1.1 - k-NN algorithm    ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Last update:  04.02.16

# Content:      This R-file creates a function performing the k-nearest-
#               neighbhor algorithm on a given input data set. 
#               The function returns a list with predicted labels and 
#               corresponding probabilities.
# 
#               Function arguments: 
# 
#               1. features:        Matrix or data frame with training data.
#               2. test:            Test data.
#               2. labels:          A vector with training labels.
#               3. k:               Parameter for the number of NN.
#               4. p:               Distance meassure (1=L1 , 2=L2 , Inf=max).
#               5. predict:         Logical (def. = FALSE) / Has to be TRUE if
#                                   there is an unlabeled test data set. 

################################################################################
### Preamble
################################################################################

### Clear workspace
#rm(list = ls())

### Set working directory


### Load Packages 
if (!require("stats")) 	 	install.packages("stats");   	library(stats)
if (!require("mvtnorm")) 	install.packages("mvtnorm"); 	library(mvtnorm)
if (!require("assertthat")) install.packages("assertthat"); library(assertthat)

### Initialize auxilliary functions

	# Mode function / Source: Stackexchange
	mode <- function(x) {
	  ux <- unique(x)
	  ux[which.max(tabulate(match(x, ux)))]
	}

################################################################################
# Section 1: Initialize k-Nearest-Neighbhor function
################################################################################

	kNN <- function( features, test = NULL , labels , k = 3, p = 2 , predict = FALSE ){
		if( predict == FALSE ) {
			# Check correct input formats
			assert_that(is.data.frame(features) || is.matrix(features))
		  	assert_that(is.vector(labels))
		  	not_empty(features)
		  	not_empty(labels)
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
			pos <- t(apply(DM,1,order))
			# Get predictions
			prediction <- lapply(1:nrow(pos),
                               function(i){ 
                                temp <- labels[ pos[i,] ][2:(k+1)] 
                                mod  <- mode(temp)
                                ep   <- length(which(temp==mod)) / length (temp)
                                return(c(mod,ep))
                                 }
                                )
			# Unlist predictions and store it in a matrix
			con 	<- matrix(unlist(prediction), ncol = 2, byrow = TRUE)
			# Prepare output
			output 	<- list(predLabels = con[,1] , prob = con[,2] )
			# Return predictions and corresponding probabilities
			return(output)
		} else if ( predict == TRUE ){ 
			# Check correct input formats
			assert_that(is.data.frame(features) || is.matrix(features))
			assert_that(is.data.frame(test) 	|| is.matrix(test))
		  	assert_that(is.vector(labels))
		  	not_empty(features)
		  	not_empty(labels)
			# Count number of points
			M 	   <- rbind(features,test)
			N 	   <- nrow(M)
			# Define arguments for distance method
			method <- c("manhattan","euclidean","maximum")
			# Convert function input to 
			if(p == Inf){
				 m <- method[3] } else {
				 m <- method[p]
			}
			# Compute distance matrix
			DM 	 <- as.matrix(dist(M, method = m , diag = FALSE, upper = TRUE) )
			# Compute positions for closest distance
			pos  <- t(apply(DM,1,order))[(nrow(features)+1):nrow(DM),]
			lab  <- 1:length(labels)
			# Get predictions
			prediction <- lapply(1:nrow(pos),
                               function(i){ 
                                temp <- labels[ intersect(pos[i,], lab) ]   [2:(k+1)]
                                mod  <- mode(temp)
                                ep   <- length(which(temp==mod)) / length (temp)
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
}

################################################################################

################################################################################