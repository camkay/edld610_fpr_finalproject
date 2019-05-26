#################################################
#### Custom function 2 (at least 2 required) ####
#################################################

name_creator <- function(scale_name, sub_name, scale_len, sub_len) {
  
  # ensure arguments of correct types
  if (!is.character(scale_name)) {
    stop("scale_name must be of type character.")
  } else if (!is.character(sub_name)) {
    stop("sub_name must of type character.")
  } else if (!is.numeric(scale_len)) {
    stop("scale_len must of type numeric.")
  } else if (!is.numeric(sub_len) & !missing(sub_len)) {
    stop("sub_len must of type numeric.")
  }
  
  # calculate the subscale length if no subscale length provided
  if (missing(sub_len)) {
    sub_len <- scale_len / length(sub_name)
  }
  
  # create a vector of subscale names to match length of the scale. `times` has 
  # to be used instead of `each` when subscales of different lengths are 
  # specified
  if (length(sub_len) == 1) {
    sub_vector <- rep(sub_name, each = sub_len)
  } else {
    sub_vector <- rep(sub_name, times = sub_len) 
  }

  # combine the scale names with the sub_scale names
  var_names <- paste(scale_name, sub_vector, sep = "_")
  
  # if length of the subscale is not equal to one, add an item identifier to the
  # column names (this segment, in particular, should be refactored)
  if (any(sub_len != 1)) {
    # get a sequence from 1 the length of a subscale for each subscale
    item_nums <- flatten_dbl(map(sub_len, seq_len))
    # repeat the sequence the number of subscales there are
    item_nums <- rep(item_nums, times = scale_len / sum(sub_len))
    # combine the variable names with the item numbers
    var_names <- paste(var_names, item_nums, sep = "_")
  }
  
  # return scale_sub
  var_names
  
}
