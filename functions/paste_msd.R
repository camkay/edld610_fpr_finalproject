#################################################
#### Custom function 10 (at least 2 required) ###
#################################################

# A simple function that combines a mean and an sd, couching the sd in 
# parantheses. For example, given a mean of 5.59 and an sd of 1.29 the output 
# will be 5.59(1.29).

paste_msd <- function(mean, sd) {
  # paste mean and sd together and surround sd with parantheses
  paste0(mean, "(", sd, ")")
}

