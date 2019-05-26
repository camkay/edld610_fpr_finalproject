#################################################
#### Custom function 4 (at least 2 required) ####
#################################################

str_alpha <- function(pattern, data, full = TRUE, na.rm = TRUE) {
  
  # ensure the pattern is a character
  if (!is.character(pattern)) {
    stop("pattern must be of type numeric.")
  # and data is a data frame
  } else if (!is.data.frame(data)) {
    stop("data must be of type data frame or tibble.")
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
  
  message(paste0("Cronbach's Alpha was calculated using ", 
                num_cols, 
                " columns: ",
                col_names,
                "."))
  
  # calculate and extract the alpha value
  alpha_output <- psych::alpha(x = data_found, warnings = FALSE, na.rm = na.rm)
  
  # return only raw alpha if FULL == FALSE
  if (full == TRUE) {
  } else if (full == FALSE) {
    alpha_output <- alpha_output[["total"]][["raw_alpha"]]  
  } else {
    warning(paste("Full must be TRUE or FALSE. \n",
                  full, "was provided. \n",
                  "Output obtined using the default (TRUE)."))
  }
  
  # return alphas
  alpha_output

}