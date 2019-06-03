#################################################
#### Custom function 3 (at least 2 required) ####
#################################################

# Takes a free response character range (e.g., "9-10 weeks") and calculates the 
# mean of the range. Returns a character by default. 


range_mean <- function(x, as.character = TRUE) {
  # ensure what is passed to the function is a character
  if (!is.character(x)) {
    stop("x must be of type character.")
  } else if (!is.logical(as.character)) {
    stop("as.character must of type logical.")
  }
  
  # attempt to find a mean for only those with a hyphen in the response
  if (grepl("-", x)) {
    # extract the range from the response
    x <- str_extract(x, "\\d*-\\d*")
    # split the string around the dash
    x <- strsplit(x, "-")[[1]]
    # convert the string to numeric and calculate the mean
    x <- mean(as.numeric(x))
    # maintains the original type if as.character = TRUE
    if (as.character == TRUE) {
      return(as.character(x))
    }
    x
  } else {
    # if no hyphen, return x
    x
  }
}

range_mean("10-12", as.character = 20)
