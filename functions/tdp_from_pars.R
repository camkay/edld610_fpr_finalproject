#################################################
#### Custom function 12 (at least 2 required) ###
#################################################

# A function for performing a t-test using a number of parameters: the mean of
# both groups, the sd of both groups, and the number of participants in both 
# groups. By default, the function performs a two-tailed test.

tdp_from_pars <- function(m1, m2, sd1, sd2, n1, n2, two.tailed = TRUE) {
  
  # check arguments
  if (!is.numeric(m1)) {
    stop("m1 must be of type numeric.")
  } else if (!is.numeric(m2)) {
    stop("m2 must be of type numeric.")
  } else if (!is.numeric(sd1)) {
    stop("sd1 must be of type numeric.")
  } else if (!is.numeric(sd2)) {
    stop("sd2 must be of type numeric.")
  } else if (!is.numeric(n1)) {
    stop("n1 must be of type numeric.")
  } else if (!is.numeric(n2)) {
    stop("n2 must be of type numeric.")
  } else if (!is.logical(two.tailed)) {
    stop("two.tailed must be of type logical.")
  }
  # calculate standard errors
  se <- sqrt(((sd1^2) / n1) + ((sd2^2) / n2))
  
  # calculate mean dif
  mean_dif <- m1 - m2
  
  # calculate t
  t <- mean_dif / se
  
  # calculate df
  df_num <- (((sd1^2) / n1) + ((sd2^2) / n2))^2
  df_den <- (((sd1^2) / n1)^2 / (n1 - 1)) + (((sd2^2) / n2)^2 / (n2 - 1))
  df <- df_num / df_den
  
  # calculate d
  d <- abs(t * sqrt((1 / n1) + (1 / n2)))
  
  # calculate p
  p <- (two.tailed + 1) * pt(-abs(t), df)
  
  # return all as list
  data.frame(t, df, d, p)
}

