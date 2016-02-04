################################################################################
####################   Problemset 4.1.2 - k-NN algorithm    ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Last update:  04.02.16

# Content:     	This R-file uses the function created in the kNN.R file
#               to predict some test data. This file contains 3 main sections
#
#               The Preamble loads/installs necessary packages, initializes 
#               functions and personal styling options (color set)
#
#               Section 1 initializes a function to create a multivaria test 
#               data set. By default it creates two classes with bimodal
#               diagonal arranged outcomes, which are not linearly separable 
#	 		
#               Section 2 performs the function on a created data set and 
#               stores a CSV file with predictions and a graphical illustration
#               of the corresponding decision boundaries.

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
if (!require("ggplot2")) 	install.packages("ggplot2"); 	library(ggplot2)
if (!require("reshape")) 	install.packages("reshape"); 	library(reshape)

### Initialize functions

    # Initialize auxilliary function to generate mvn. data / 
    # SOURCE: Modifiaction from handout 1
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

    # Mode function / 
    # SOURCE: Stackexchange - http://goo.gl/5TBuR0 (URL shortened)
    mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }

    # K-NN function - Predictes the k nearest neighbhour according to input specification
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

###Styling options

co <- 1/255 
pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.oragange   <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )

################################################################################
# Section 1: Initialize function to create test data
################################################################################

# Function generating data for classification ( Author - Denitsa Panova )
genSpirals <- function(N = 2000, degrees = 570, location = 90, blend = 0.2) {
  
    # Define variables and convert degrees to radiant
    degrees2rad <- (2*pi)/360 
    # Set location, how far away from 00 the spiral starts
    location <- location * degrees2rad 
    
    N1 <- floor(N/2)
    N2 <- N-N1
    
    # Spiral 1 - Indicate it by 0 in V3
    n  <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
    d1 <- t(rbind(-1*n*cos(n)+runif(N1)*blend, sin(n)*n+runif(N1)*blend, rep(0,N1)))
    
    # Spiral 2 - Indicate by 1 in V3
    n <- as.vector(location+sqrt(runif(N1))*degrees*degrees2rad)
    d2 <-t(rbind(n*cos(n)+runif(N1)*blend, -1*sin(n)*n+runif(N1)*blend, rep(1,N1))) 
    
    # Combine and rename the data 
    data <- data.frame(rbind(d1, d2))
    names(data) <- c("x1", "x2", "y")
   
   	# Return data set
    return(data)

}

################################################################################
# Section 2: Create test, predict and save output
################################################################################

# Create data set with default arguments 
train <- genSpirals()

# Split generated data for function input
X <- train[c("x1","x2")]
Y <- train$y

# Classify data with 5-NN and euclidian distance
classification <- kNN( features = X, test = NULL , labels = Y,
							  k = 5, 	p = 2,	  predict = FALSE )

# Prepare output
output <- as.data.frame(cbind(X,Y,classification))

# Save prediction data
write.table(output, file = "predictions.csv", row.names=FALSE, col.names=TRUE) 

# Plot data and decision boundaries. Modus operandi is adapted from Stackexchange,
# SOURCE: http://goo.gl/GZomWl (URL shortened)

# Create some "test" data set to plot the decision boundaries
x 	  <- seq( min(train$x1), max(train$x1), length = 50)
y 	  <- seq( min(train$x2), max(train$x2), length = 50)
n 	  <- length(x)
test  <- expand.grid( x1 = x, x2 = y )
 
# Run  classification on grid data 
classification <- kNN( features = X , test = test , labels = Y, 
							  k = 15, 	 p = 2 ,   predict = TRUE )

# Transform data for ggplot argument input
conversion <- ifelse(classification$predLabels == 0 , -1 , 1 )

# Create matrix for plot input
mat <- transform( melt( matrix( conversion, n ) ), x = x[X1], y = y[X2] )

# Plot the data and the corresponding decision boundaries
plot <- ggplot(mat, aes( x , y , z = value)) + 
		  geom_contour(colour = pers.green)  + 
		  geom_point(aes( x1 , x2 , colour = y, z = NULL ), data = train ) +
		  theme_bw() +
		  xlab("") 	 +
		  ylab("")   +
		  theme( legend.position = "none" )

# Save plot and end file
ggsave( plot , file = "plot.pdf" )

################################################################################

################################################################################