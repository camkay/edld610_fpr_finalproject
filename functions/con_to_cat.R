#################################################
#### Custom function 9 (at least 2 required) ####
#################################################

# This function takes one required argument (i.e., a numeric column) and 
# quartile splits it to create a new character vector (i.e., a vector consisting 
# of three strings: low, mid, and high). By default, the result is converted to 
# a factor and NAs are removed.

con_to_cat <- function(column, na.rm = TRUE, as.a.factor = TRUE) {
  
  # argument check
  if (!is.numeric(column)) {
    stop("column must of type numeric.")
  } else if (!is.logical(as.a.factor)) {
    stop("as.a.factor must be of type logical.")
  } else if (!is.logical(na.rm)) {
    stop("na.rm must of type logical.")
  }
  
  # calculate split values
  low_split  <- quantile(column, .25, na.rm = na.rm)
  high_split <- quantile(column, .75, na.rm = na.rm)
  
  # indicate where the data will be split
  message(paste0("Low and middle are split at ",  low_split, "."))
  message(paste0("Middle and high are split at ", high_split, "."))
  
  # create a character vector, splitting the continuous variable
  cat_var <- case_when(column < low_split  ~ "Low",
                       column > high_split ~ "High",
                       TRUE                ~ "Mid")
  
  # convert the variable to a factor and rearrange the levels
  if (as.a.factor == TRUE) {
    cat_var <- factor(cat_var, levels = c("Low", "Mid", "High"))
  }
   
  # return categorical variable  
  cat_var
}

