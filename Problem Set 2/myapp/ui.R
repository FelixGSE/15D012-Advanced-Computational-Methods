###################################################################################################

###################################################################################################

# Author:       Felix Gutmann
# Course:       15D012 - Advanced Computational Methods
# Last update:  28.01.16
# Content:      Problemset 2 - User interface for shiny application 

###################################################################################################
### Praeamble
###################################################################################################

### Clear workspace
#rm(list = ls())

### Load Packages 
if (!require("shiny")) 	  install.packages("shiny"); 	library(shiny)

###################################################################################################

###################################################################################################

# Set column width for sub panels in sidebar panel
b <- 6

# Launch shiny interface
shinyUI(
	# Create page
	fluidPage(
    # Set page title
    titlePanel("Linear discriminant classification with simmulated loan data"),
    # Introduce some extra spacing
    br(),
	  # Create the side bar panel for dynamic data manipulation
  	sidebarPanel(
  	 # Sub panel 1: Means and standard deviation for class APPROVED
  	 wellPanel(
      # Header sub panel 1
      h4("Data manipulation options for class Approved"),
      # Row 1: Mean and standard deviation for PI - ratio
      fluidRow(
  	 	  column(b, sliderInput( inputId = 'm11', label = "Mean PI-ratio", 
                                   min = 1,       max = 300, 
                                 value = 10,     step = 10)),
        column(b, offset = 0, 
                  sliderInput( inputId = 's11', label = "Sigma PI-ratio", 
                                   min = 1,       max = 50, 
                                 value = 5,      step = 5))
              ),
      # Row 2: Mean and standard deviation for Solvency
  	  fluidRow(
  	 	  column(b, sliderInput( inputId = 'm12', label = "Mean Solvency", 
                                   min = 1,       max = 300, 
                                 value = 150,    step = 10)),
        column(b, offset = 0, 
                   sliderInput(inputId = 's12', label = "Sigma Solvency", 
                                   min = 1,       max = 50, 
                                 value = 20,     step = 5))
     )),
    # Sub panel 2: Means and standard deviation for class DENIED
    wellPanel(
     # Header sub panel 2:
     h4("Data manipulation options for class Denied"),
  	 # Row 1: Mean and standard deviation for PI - ratio
  	 fluidRow(
  	 	  column(b, sliderInput( inputId = 'm21', label = "Mean Solvency", 
                                   min = 1,       max = 300, 
                                 value = 10,     step = 10)),
        column(b, offset = 0, 
                  sliderInput(inputId = 's21',  label = "Sigma Solvency", 
                                  min = 1,        max = 50, 
                                value = 5,       step = 5))
        ),
     # Row 2: Mean and standard deviation for Solvency
  	 fluidRow(
  	 	  column(b, sliderInput( inputId = 'm22', label = "Mean PI-ratio", 
                                   min = 1,       max = 300, 
                                 value = 100,    step = 10)),
        column(b, offset = 0, 
                  sliderInput( inputId = 's22', label = "Sigma PI-ratio", 
                                   min = 1,       max = 50, 
                                 value = 30,     step = 5))
        )),
    # Sub panel 3: Means and standard deviation for class Undecided
  	 wellPanel(
     # Header sub panel 3:
     h4("Data manipulation options for class Denied"),
     # Row 1: Mean and standard deviation for PI - ratio
  	 fluidRow(
  	 	  column(b, sliderInput( inputId = 'm31', label = "Mean Solvency", 
                                   min = 1,       max = 300, 
                                 value = 15,     step = 10)),
        column(b, offset = 0, 
                  sliderInput( inputId = 's31', label = "Sigma Solvency", 
                                   min = 1,       max = 50, 
                                 value = 1,      step = 5))
        ),
     # Row 2: Mean and standard deviation for Solvency
  	  fluidRow(
  	 	  column(b, sliderInput( inputId = 'm32', label = "Mean PI-ratio)", 
                                   min = 1,       max = 300, 
                                 value = 250,    step = 10)),
        column(b, offset = 0, 
                   sliderInput(inputId = 's32', label = "Sigma PI-ratio", 
                                   min = 1,       max = 50, 
                                 value = 15,     step = 5))
        ))
  	 ),
    # Main panel: Embed plot and confusion matrix
    mainPanel(
    # Header above the the plot
    h3("Scatterplot with corresponding discriminant functions"),
    fluidRow(column(10,plotOutput('ClassificationPlot'))),
    # Header above the confusion matrix - table position: below
    h3("Confusion matrix for linear discriminant classification"),
    fluidRow(column(3,tableOutput('ConfussionTable')))
  )
))

###################################################################################################

###################################################################################################
