#################################################
#### Custom function 11 (at least 2 required) ###
#################################################

library(tidyverse)

# Replaces a vector of p-values with a vector of asterisks. By default a p-value
# of .05 is replaced with one asterisk, a p-value of .01 is replaced with two
# asterisks, and a p-value of .001 is replaced with three asterisks. 

p_to_ast <- function(column, thresholds = c(.05, .01, .001)) {
  
  # argument check
  if (!is.numeric(column)) {
    stop("column must of type numeric.")
  }
  
  # create a character vector, splitting the continuous variable
  ast <- case_when(column < thresholds[3] ~ "***",
                   column < thresholds[2] ~  "**",
                   column < thresholds[1] ~   "*",
                   TRUE                   ~    "")
  
  # return categorical variable  
  ast
}


