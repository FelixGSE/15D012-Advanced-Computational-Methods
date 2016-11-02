Two arguments added to the function:

1) test:  A test set / Must be class data frame / Default = NULL

2) trace: Logical, indicating whether function should show the accuracy
		  of each tree in each itteration 


Modification in output:

The function outputs a named list with the following content

1) predLabels:		Predicted labels for training set

2) errorTrace:		accuracy for each tree in each iteration

3) PredLabelsTest:	Predicted labels for test set