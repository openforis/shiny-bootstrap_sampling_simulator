################################################################
# BOOTSRAPPING ALGORITM
# Lauri Vesa, FAO. 1 March 2019
# updated LV: 9 April 2021
# the script takes random samples from 'sampleplots' data, with given interval ('s_step') and min/max limits
# and 'n_simulations' times at each case, and computes statistics (mean, variance, standard error, relative standard error, confidence intervals 95%)
# and makes graphs and non-linear model ('m1') of the form 
# 'c1*nplots^c2' where c1 and c2 are model parameters, nplots=number of samples.
# More about applied method at  http://www.fs.fed.us/emc/rig/Plot-GEM/
################################################################


bootstrap_results <- function(sampleplots, min_s_size, max_s_size, s_step, n_simulations, ci_level) {
  
 if (nrow(sampleplots)>5) {

  plots <- sampleplots

  # s_step: sampling interval
  # n_simulations: maximum number of sampling simulations in each sampling point
   
  a_nplots <- as.numeric(c())
  a_mean   <- as.numeric(c())
  a_var    <- as.numeric(c())
  a_se     <- as.numeric(c())
  a_error  <- as.numeric(c())
  a_ci     <- as.numeric(c())
  ja_mean  <- as.numeric(c())
  ja_var   <- as.numeric(c())
  ja_se    <- as.numeric(c())
  ja_error <- as.numeric(c())
  ja_ci    <- as.numeric(c())

  n <- 1
  for(i in seq(min_s_size, max_s_size, s_step)) {
    t <- qt(ci_level + (1 - ci_level)/2, i)
    for(j in seq(1, n_simulations)) {
      sa <- sample(plots$input_var, i, replace=F)
      ja_mean[[j]]   <- mean(sa, na.rm=TRUE )
      ja_var[[j]]    <- var(sa)
      ja_se[[j]]     <- sqrt(var(sa)/(i-1)) 
      ja_error[[j]]  <- t * 100 * sqrt(var(sa)/(i-1)) / mean(sa)
      ja_ci[[j]]     <- t * ja_se[[j]]
    } 
    a_mean[[n]]   <- mean(ja_mean, na.rm=TRUE)
    a_nplots[[n]] <- i
    a_var[[n]]    <- mean(ja_var,   na.rm=TRUE)
    a_se[[n]]     <- mean(ja_se,    na.rm=TRUE)
    a_error[[n]]  <- mean(ja_error, na.rm=TRUE)
    a_ci[[n]]     <- mean(ja_ci,    na.rm=TRUE)

    n <- n + 1
  }

  a <- data.frame(a_nplots)
  a <- cbind(a,a_mean)
  a <- cbind(a,a_var)
  a <- cbind(a,a_se)
  a <- cbind(a,a_error)
  a <- cbind(a,a_ci)
  a$ci_lower <- a$a_mean - a$a_ci
  a$ci_upper <- a$a_mean + a$a_ci

  
  return(a)

 } else (return(NULL))
}