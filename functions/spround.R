#################################################
#### Custom function 6 (at least 2 required) ####
#################################################

spround <- function(x, digits = 2, leading0 = TRUE) {
  
  # check x argument
  if (!is.numeric(x)) {
    
    # check if x is a character
    if (is.character(x)) {
      
      # if x is a character, coerce to numeric if possible.
      x <- as.numeric(x)
      
      # if the result of the coercion is na, stop the function
      if(any(is.na(x))) {
        stop("X was a character or a character vector, but could not be coerced to type numeric.")
      }
      
      # inform user that x was coerced
      warning("X was coerced to type numeric")
      
    # stop the function if x is neither a numeric or a character
    } else {
      stop("X must be of type numeric or character.")
    }
  } 
  
  # check that digits is numeric
  if (!is.numeric(digits) | length(digits) != 1) {
    stop("Digits must be of type numeric and of length one.")
  }
  
  #  check that leading0 is logical and of length one
  if (!is.logical(leading0) | length(leading0) != 1) {
    stop("leading0 must be of type logical (i.e., true or false) and of length one.")
  }
  
  # round x to the number of digits
  x <- round(x, digits)
  
  # format the number for knitting
  x <- sprintf(paste0("%.", digits, "f"), x)
  
  # if leading0 == FALSE, drop the leading zero
  if (leading0 == FALSE) {
    x[grepl( "^0", x)]  <- sub("0", "", x[grepl( "^0", x)])
    x[grepl("^-0", x)]  <- sub("0", "", x[grepl("^-0", x)])
  }
  
  # return x
  return(x)
}