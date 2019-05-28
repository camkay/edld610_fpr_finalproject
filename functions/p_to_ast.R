#################################################
#### Custom function 11 (at least 2 required) ###
#################################################

p_to_ast <- function(column) {
  
  # argument check
  if (!is.numeric(column)) {
    stop("column must of type numeric.")
  }
  
  # create a character vector, splitting the continuous variable
  ast <- case_when(column < .001 ~ "***",
                   column < .01  ~  "**",
                   column < .05  ~   "*",
                   TRUE          ~   "")
  
  # return categorical variable  
  ast
}
