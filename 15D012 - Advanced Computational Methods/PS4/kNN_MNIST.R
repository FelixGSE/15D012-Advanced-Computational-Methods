################################################################################
####################   Problemset 4.1.3 - k-NN algorithm    ####################
################################################################################

# Author:       Felix Gutmann
# Programm:		Barcelona graduate school of economics - M.S. Data Science 
# Course:       15D012 - Advanced Computational Methods (Term 2)
# Last update:  04.02.16

# Content:     	This R-file uses k-NN for classification of the MNIST
#				data set. The applied knn() function comes from the class package.

################################################################################
### Preamble
################################################################################

### Clear workspace
#rm(list = ls())

### Set working directory


### Load packages
if (!require("class")) install.packages("class"); library(class)

### Load data (training and test data)
training <- read.csv( "MNIST_training.csv", header = FALSE )
test 	 <- read.csv( "MNIST_test.csv",	    header = FALSE )

################################################################################
# Section 1: Initialize function to create test data
################################################################################

# Prepare data and specify parameter
X00 <- training[, !(colnames(training) %in% c("V1"))]
Y00 <- training$V1 
X01 <- test
k   <- 3

# Fit model
classification <- knn(X00,X01,Y00,3)

# Save predictions
write.table(classification, 
			file 	  = "MNIST_predictions.csv",
			row.names = FALSE, 
			col.names = FALSE) 

################################################################################

################################################################################