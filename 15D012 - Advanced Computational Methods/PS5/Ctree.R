################################################################################

################################################################################

### Clear workspace
#rm(list = ls())

### Load packages

### Functions
  
  # Split data 
  splitData <- function( data , col , threshold ){
    # define temp column
    temp <- data[,col]
    # Slice data according to threshold
    if( is.character(threshold) ){
    s01  <- subset(data, temp == threshold )
    s02  <- subset(data, temp !=  threshold )  
    } else {
    s01  <- subset(data, temp >= threshold )
    s02  <- subset(data, temp <  threshold )
    }
    # Design output list
    sets <- list( s01 = s01, s02 = s02  )
    # Return output
    return(sets)
  }
  
  # Compute empirical probabilites
  empProb  <- function( data ){
    n <- length( data )
    freqData <- as.data.frame( table( data ) ) 
    maxPos   <- which.max( freqData[,2] )
    c <- as.numeric(freqData[maxPos,1])
    f <- as.numeric(freqData[maxPos,2])
    p <- c / n
    output <- list( item = c , prob = p )
    return(output)
  }
  
  # Entropy function
  entropy <- function( data , y ){
    # Compute number of rows
    n         <- nrow( data )
    # Relative frequency per response
    count     <- as.data.frame( table( data[,y] ) ) 
    # Compute probability
    prob      <- ( count$Freq / n ) 
    # Stepwise entropy
    ent       <- prob * log(prob) / log ( 2 )
    # Final entropy
    entropy   <- sum( -ent ) 
    # Return entropy
    return(entropy)
  }

################################################################################

################################################################################

cTree <-function( data, y ){
  # Compute dimensions of input data
  M <- ncol( data ) - 1
  N <- nrow( data )
  # Set global gain counter
  ggain <- list( gain = 0 )
  # Set current gain counter
  cgain <- entropy( data , y )
  # Col names to loop over
  cols  <- setdiff( names(data) , y )
  # Compute column wise splits
  for( i in cols ){
    # Get unique column items
    temp  <- unique( data[, i] ) 
    # Compute length of unique values
    ltemp <- length( temp ) 
    # Check each value as possible split
    for( v in 1:ltemp ){
      # Current value
      value <- temp[v]
      # Slice data 
      split <- splitData( data, i , value )
      # Get left and right split
      left  <- split$s01
      right <- split$s02
      # Compare child and parent
      p     <- nrow( left ) / N 
      # Compute current score
      score <- cgain - p * entropy( left , y ) - ( 1 - p ) * entropy( right , y )
      # Check if further recursion are necessary
      if( score > ggain$gain & nrow( left ) > 1 & nrow( right ) > 1 ){
        # Update ggain
        ggain <- list( gain   = score, 
                       column = i, 
                       value  = value, 
                       set1   = left, 
                       set2   = right
        )
      }
    }
  }
  # Run recursion if gain is positive 
  if ( ggain$gain > 0 ){
    # Split set 1
    recL01 <- cTree( ggain$set1 , y )
    # Split set 2
    recR01 <- cTree( ggain$set2 , y )
    # Design output
    o01    <- list( action    = "split" , 
                    column    = ggain$column, 
                    value     = ggain$value,
                    pathLeft  = recL01, 
                    pathRight = recR01
    )
    # Return output
    return( o01 )
    # No further gain
  } else {
    # Compute probablity
    nodeProb <- empProb( y )
    # Design output
    o02      <- list( isLeaf  = "TRUE", 
                      label   = nodeProb$item, 
                      prob    = nodeProb$prob
    )
    # Return output
    return( o02 )
  }
}

################################################################################

################################################################################