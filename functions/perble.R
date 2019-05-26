#################################################
#### Custom function 7 (at least 2 required) ####
#################################################

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
