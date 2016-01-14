###################################################################################################

###################################################################################################

# Author:       Felix Gutmann
# Course:       15D012 - Advanced Computational Methods
# Last update:  15.01.16
# Content:      Problemset 1 - Exercise 1

###################################################################################################
### Praeamble
###################################################################################################

### Clear workspace
rm(list = ls())

### Load Packages 
if (!require("mvtnorm")) install.packages("mvtnorm"); library(mvtnorm)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

### Set working directory

### Initialize auxilliary function to generate mvn. data / Source: modifiaction from handout 1
mvsamp <- function(rho, sdX, sdY,n = 1,  mean=c(0,1),seed , sigmaXY=diag(2)) {
  covTerm  <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 2, 2, byrow = TRUE)
  set.seed(seed)
  draws    <- as.data.frame(rmvnorm(n, mean = mean, sigma = sigmaXY))
  colnames(draws) <- c("V1","V2")
  return(draws)
}

###################################################################################################
### Exercise 1
###################################################################################################

# Initialize function for generating dataset with four possible point clouds and two categories
genData <- function(class1 =10, class2 =10, save = TRUE, export=TRUE,
                    rho1   = 1, sdX1  = 1 , sdY1 = 1, mean1 = c(0,1), seed1 = 1, sigmaXY1 = diag(2),
                    rho2   = 1, sdX2  = 1 , sdY2 = 1, mean2 = c(0,1), seed2 = 1, sigmaXY2 = diag(2),
                    rho3   = 1, sdX3  = 1 , sdY3 = 1, mean3 = c(0,1), seed3 = 1, sigmaXY3 = diag(2),
                    rho4   = 1, sdX4  = 1 , sdY4 = 1, mean4 = c(0,1), seed4 = 1, sigmaXY4 = diag(2)
                    )
{
  # Generate data according to specified parameters
  samp      <- cbind(mvsamp(rho1, sdX1, sdY1, class1, mean1, seed1, sigmaXY1),Category="CAT1")
  diag      <- cbind(mvsamp(rho2, sdX2, sdY2, class1, mean2, seed2, sigmaXY2),Category="CAT1")
  up        <- cbind(mvsamp(rho3, sdX3, sdY3, class2, mean3, seed3, sigmaXY3),Category="CAT2")
  right     <- cbind(mvsamp(rho4, sdX4, sdY4, class2, mean4, seed4, sigmaXY4),Category="CAT2")
  # Set up data set
  sim.dat   <- as.data.frame(rbind(samp,diag,up,right))
  # OPTION 1: Save data as CSV - TRUE by default
  if(save==TRUE) 
    { 
    write.table(sim.dat, file = "benchmark.csv",row.names=FALSE, na="",col.names=TRUE, sep=";") 
    }
  # OPTION 2: Save plot of generated data - TRUE by default
  if(export==TRUE)
    {
    # Save ggplot graph
      p01 <- ggplot(data = sim.dat, 
             aes(x = V1, y = V2, colour= Category)) + 
             geom_point() +
             xlab("X") +
             ylab("Y") +
             theme_bw()
    # Export object
      ggsave(p01,file="benchmark.pdf")
    }
  return(sim.dat)
}

# Generate some example dataset
exData <-genData(50, 50,TRUE,TRUE,
           1, 2, 1,  c( 0,  0), 100, diag(2),
           1, 1, 1,  c(15, 15), 100, diag(2),
           1, 1, 1,  c(10, 10), 100, diag(2),
           1, 1, 1,  c(20, 20), 100, diag(2)
           )

###################################################################################################
###### End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE # End CODE ######
###################################################################################################