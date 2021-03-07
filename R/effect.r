#' Calculate nonparametric effect size vector
#' 
#' \code{distr.effect} calculates the effect vector of two distributions
#' @param x A vector of numbers for the first distribution
#' @param y A vector of numbers for the second distribution
#' @param mid Toggles whether to give the distribution or the median
# returns a vector of standardized effect sizes
# recycles if x,y are different sizes
distr.effect <- function(x,y, mid=FALSE){
  if(mid == FALSE){ 
      return( distr.diff(x,y)/distr.mmad(x,y) )
  } else {
      return(median (distr.diff(x,y)/distr.mmad(x,y) ) )
  }
}


