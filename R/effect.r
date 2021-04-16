#' Calculate nonparametric effect size vector
#' 
#' \code{distE.effect} calculates the effect vector of two distributions
#' @param x A vector of numbers for the first distribution
#' @param y A vector of numbers for the second distribution
#' @param mid Toggles whether to give the distribution or the median
#' @export distE.effect
#' @import stats

# returns a vector of standardized effect sizes
# recycles if x,y are different sizes
distE.effect <- function(x,y, mid=TRUE){
  if(mid == TRUE){ 
    return(median (distE.diff(x,y)/distE.mmad(x,y) ) )
 } else {
    return( distE.diff(x,y)/distE.mmad(x,y) )
  }
}


