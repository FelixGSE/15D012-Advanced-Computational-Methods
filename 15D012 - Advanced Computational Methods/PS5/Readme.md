REMARKS ON THE PROBLEMSET

The function has a lot of issues at this point. However,
with it limited possibilities it is producing a tree as an output. 
This file lines out the current status and function usage. 

DETAILED EXPLANATION

The function is not fully developed at this point. There are the following main issues:

1) Function can NOT deal with formulas 
2) Function can NOT predict a test data set
3) Function can NOT handle desired function arguments
4) Function does NOT use the same arguments

Right now the function manages to produce RECURSIVELY
a decision tree based on an input data set and a response 
variable using an entropy function. The function is inspired by
a python implementation (Source, at the end of this file). The
function was checked with a simple data set. 

FUNCTION EXPLANATION:

	cTree( data , y )

	1) data: data is an input data frame
	2) y:	 y is a STRING indicating outcome

The function outputs a list containing information about the 
tree structure. In order to make the output more readable one
could use the following PSEUDO EXAMPLE structure:

tree 	  <- cTree( data = data, y = "outcome")
structure <- as.data.frame( unlist( tree ) )

SOURCE:

The current implementation follows:

Link:		http://goo.gl/zBTUkv
