#################################################
#### Custom function 8 (at least 2 required) ####
#################################################

# Streamlines `length(unique(x))` into on function. Relies on the warnings 
# produced by `length()` and `unique()`.

lenique <- function(x) {
  # calculate the length of unique values
  length(unique(x))
}
