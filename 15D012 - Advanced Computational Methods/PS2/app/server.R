################################################################################

################################################################################

# Author:       Felix Gutmann
# Course:       15D012 - Advanced Computational Methods
# Last update:  28.01.16
# Content:      Problemset 2 - Server of shiny application

################################################################################
### Preamble
################################################################################

### Clear workspace
#rm(list = ls())

### Load Packages 
if (!require("shiny"))    install.packages("shiny");    library(shiny)
if (!require("mvtnorm"))  install.packages("mvtnorm");  library(mvtnorm)
if (!require("ggplot2"))  install.packages("ggplot2");  library(ggplot2)

### Set working directory

### Initialize auxilliary functions

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

  # Initialize modified loanData function / (c.p. handout 1)
  loanData <- function(  noApproved,  noDenied,  Undecided,
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

###Styling options

co <- 1/255 
pers.green      <- rgb( co *  14 ,  co * 105 , co *  90 )
pers.blue       <- rgb( co *  22 ,  co *  54 , co *  92 )
pers.red        <- rgb( co *  99 ,  co *  37 , co *  35 )
pers.gray       <- rgb( co * 150 ,  co * 150 , co * 150 )
pers.oragange   <- rgb( co * 186 ,  co *  85 , co *  59 )
pers.beige      <- rgb( co * 196 ,  co * 189 , co * 151 )


################################################################################

################################################################################

shinyServer(function(input, output, session) {

output$ConfussionTable <- renderTable({
# Create dataset
noApproved <- 50
noDenied   <- 50
Undecided  <- 50  
noObs      <- noApproved + noDenied + Undecided

mu01 <- c(input$m11,input$m12)
mu02 <- c(input$m21,input$m22)
mu03 <- c(input$m31,input$m32)

s01 <- c(input$s11,input$s12)
s02 <- c(input$s21,input$s22)
s03 <- c(input$s31,input$s32)

loanDf <- loanData(noApproved, noDenied, Undecided,
                   mu01, mu02, mu03,
                   s01, s02, s03,
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

# Compute confusion matrix 
confusion.matrix <- prop.table(table(loanDf$status, predict),1)
})

output$ClassificationPlot <- renderPlot({
# Create dataset
noApproved <- 50
noDenied   <- 50
Undecided  <- 50  
noObs      <- noApproved + noDenied + Undecided

mu01 <- c(input$m11,input$m12)
mu02 <- c(input$m21,input$m22)
mu03 <- c(input$m31,input$m32)

s01 <- c(input$s11,input$s12)
s02 <- c(input$s21,input$s22)
s03 <- c(input$s31,input$s32)

loanDf <- loanData(noApproved, noDenied, Undecided,
                   mu01, mu02, mu03,
                   s01, s02, s03,
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

# Compute confusion matrix 
confusion.matrix <- prop.table(table(loanDf$status, predict),1)

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
                                values = c("Boundary 1" = pers.oragange  , "Boundary 2" = pers.beige , "Boundary 3" = "yellow",
                                            "Approved"  =  pers.green  , "Denied"     = pers.red , "Undecided"  = pers.gray
                                            )
                              )
# Save plot
  plot01})

})

################################################################################

################################################################################
