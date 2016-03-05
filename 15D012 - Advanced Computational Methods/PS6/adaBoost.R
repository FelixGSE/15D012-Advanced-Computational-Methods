################################################################################
####################   Problemset 6 - adaboost algorithm    ####################
################################################################################

# Author:       Felix Gutmann
# Programm:     Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Last update:  05.03.16

################################################################################
### Preamble
################################################################################

### Clear workspace
rm(list = ls())

### Set working directory

### Load Packages 
if (!require("rpart")) 	 	install.packages("rpart");   	library(rpart)

### Initialize auxilliary functions

################################################################################
# adaboost function
################################################################################

adaBoost <- function( formula , data , test = NULL, depth = 1, noTrees = 1, 
                      trace = TRUE ) 
{ 
  
  # Enjure correct scoping 
  environment(formula) <- environment( ) 
  # Get name of response variable
  Nresponse <- all.vars( formula )[1]
  
  # Check for correct format of training
  if( is.data.frame( data ) == FALSE ){
    stop("Error: training must be an object of class data frame")
  } 
  # Check for correct format of test if present
  if( is.null( test ) == FALSE & is.data.frame(test) == FALSE ){ 
    stop("Error: The argument test must be an object of class data frame")
  }
  # Check for trace
  if( is.logical( trace ) == FALSE ){
    stop("Error: trace must be class logical")
  }
  # Check if data are empty - training
  if( ncol(data) == 0 | nrow(data) == 0 ){
    stop("Error: The argument training can't be empty")
  } 
  # Check if data are empty - test if present
  if( is.null( test ) == FALSE & ncol(test) == 0 | nrow(test) == 0 ){
    stop("Error: The argument test can't be empty")
  } 
  # Convert response variable if necessary and check if empty
  if( Nresponse %in% names(data) == FALSE  ){ 
    stop("Error: Response varaible is not an element of data") } 
  # Check if variable is a factor - Convert if not
  y <- data[ , Nresponse ]
  
  if ( is.factor( y ) == FALSE ) { 
    data$y <- as.factor( y )
    warning("Warning: Response converted to factors")
  }
  
  # Compute domensions and initialise weights
  N       <- nrow( data )
  w       <- rep( (1/N) , N )
  # Initialize alpha and classification 
  global.alpha   <- rep( NA, noTrees )
  classifiers    <- matrix( NA, nrow = N, ncol = noTrees )
  accuracy       <- rep( NA, noTrees )
  
  # Initialize test storage obeject in case of testing
  if( is.null( test ) == FALSE ){ 
    M <- nrow( test )
    classifiers.test <- matrix( NA, nrow = M, ncol = noTrees )
  }
  
  # Run learning procedure
  for( i in 1:noTrees){
    # Compute model with current weights
    temp.model <- rpart( formula ,
                         data     = data , 
                         weights = w , 
                         maxdepth = depth ) 
    # Prediction current model
    prediction <- predict( temp.model, newdata=data, type = 'class') 
    # Compute errors and update parameters 
    error.ind  <- ifelse( prediction == y , 0, 1 )
    errors     <- sum( w * error.ind ) / sum( w )
    alpha      <- log( ( 1 - errors ) / errors )
    w          <- w * exp( alpha * error.ind ) 
    
    # Store variables
    global.alpha[i]   <- alpha 
    classifiers[,i]   <- ifelse( as.numeric( prediction ) == 2, 1, -1 )
    acc <- sum( error.ind )  / length( y )
    accuracy[i]       <- acc
    
    if( trace == TRUE ){
      cat( 'Training accuracy tree ', i , ': ' , acc , '\n' )
    } 
    
    # Get prediction with current model for test set if specified
    if( is.null( test ) == FALSE ){ 
      classifiers.test[,i] <- ifelse( as.numeric( 
        predict( temp.model, 
                 newdata = test, 
                 type = 'class' ) 
      ) == 2, 1, -1)
    }
  }
  # Design final classifier - If test set available add to output
  if( is.null( test ) == FALSE ){
    final.prediction.training <- sign( classifiers      %*% global.alpha ) 
    final.prediction.test     <- sign( classifiers.test %*% global.alpha ) 
    output <- list( predLabels     = as.numeric( final.prediction.training ),
                    errorTRACE     = accuracy, 
                    predLabelsTest = as.numeric( final.prediction.test ) )
    # If no test set output only training set predictions
  } else { 
    final.prediction.training <- sign( classifiers      %*% global.alpha ) 
    output <- list( predLabels = as.numeric( final.prediction.training ),
                    errorTRACE = accuracy
    )
  }
  
  # Return output
  return(output)
  
} 

################################################################################

################################################################################