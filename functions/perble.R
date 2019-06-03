#################################################
#### Custom function 7 (at least 2 required) ####
#################################################

# A variant of table that, in addition to values counts, calculates the 
# percentage of the total values that a given value represents. s

perble <- function(column){
  
  # create a table from the passed column
  temp <- table(column)
  
  # calculate percentage of each member of that table
  percent_table <- rbind(temp, temp / sum(as.vector(temp)) * 100)
  
  # name the rows of the table
  rownames(percent_table) <- c("Count", "Percent")
  
  # return the  percent_table
  percent_table
}
