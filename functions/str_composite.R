#################################################
#### Custom function 5 (at least 2 required) ####
#################################################

# Creates a composite vector from names of columns in a dataframe that match a
# user-provided string. By default, the composite is calculated by row-wise 
# averaging, however the option is available to calculate the composite using
# row-wise summation. Similar to str_alpha, the function also
# messages the user, telling them the names of the columns that were
# used to calculate the composite. If only four columns were used, all column
# names are returned. If greater than four columns were used, the first three
# columns are named, followed by "and * more."

str_composite <- function(pattern, data, sum = FALSE) {
  
  # ensure the pattern is a character
  if (!is.character(pattern)) {
    stop("pattern must be of type character.")
  # and data is a data frame
  } else if (!is.data.frame(data)) {
    stop("data must be of type data frame or tibble.")
  } else if (!is.logical(sum)) {
    stop("sum is not of type logical.")
  }
  
  # drop columns not including the pattern
  data_found <- str_cols_find(pattern, data, return = "data")
  
  # message user how the composites were created
  num_cols <- ncol(data_found)
  
  if (num_cols == 0) {
    stop("No columns matched the provided string.")
  } else if (num_cols  <= 4) {
    col_names <- paste(names(data_found), collapse = ", ")
  } else {
    col_names <- paste0(paste(names(data_found)[1:3], collapse = ", "), ", and ", num_cols - 3, " more")
  }
  
  message(paste0("Row ",
                 if_else(sum == FALSE, "means", "sums"),
                 " were calculated using ", 
                num_cols, 
                " columns: ",
                col_names,
                "."))
  
  # calculate row means or row sums
  if (sum == FALSE) {
    rowMeans(data_found)
  } else {
    rowSums(data_found)  
  }
}

