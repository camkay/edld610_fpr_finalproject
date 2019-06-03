#################################################
#### Custom function 1 (at least 2 required) ####
#################################################

str_cols_find <- function(pattern, data, return = "logical") {
  
  # ensure pattern is a character and the data is a dataframe
  if (!is.character(pattern)) {
    stop("Pattern must be of type character.")
  } else if (!is.data.frame(data)) {
    stop("Data must of type dataframe/tibble.")
  }

  # format x for output
  if (return == "logical") {
    x <- grepl(pattern, names(data))
  } else if (return == "numbers") {
    x <- grep(pattern, names(data))
    
# Why is it grep() here when it was grepl() in line 16? Does it matter?
    
  } else if (return == "names" ) {
    x <- grep(pattern, names(data), value = TRUE)
  } else if (return == "data") {
    x <- data[, grep(pattern, names(data))]
  } else {
    warning(paste0(return, " is not a recognized return type. \"logical\" used instead. \n", 
                   "Other return options include: \"numbers\", \"names\", and \"data\"."))
    x <- grepl(pattern, names(data))
  }
  
  # return x
  x

}




