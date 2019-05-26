#################################################
#### Custom function 3 (at least 2 required) ####
#################################################

range_mean <- function(x) {
  # ensure what is passed to the function is a character
  if (!is.character(x)) {
    stop("x must be of type character.")
  }
  
  # attempt to find a mean for only those with a hyphen in the response
  if (grepl("-", x)) {
    # extract the range from the response
    x <- str_extract(x, "\\d*-\\d*")
    # split the string around the dash
    x <- strsplit(x, "-")[[1]]
    # convert the string to numeric and calculate the mean
    x <- mean(as.numeric(x))
    # maintaining the original type, convert the result back to a character
    as.character(x)
  } else {
    # if no hyphen, return x
    x
  }
}