#################################################
#### Custom function 12 (at least 2 required) ###
#################################################

tdp_from_pars <- function(m1, m2, sd1, sd2, n1, n2, two.tailed = TRUE) {
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




