#' Calculate pooled dispersion via MMAD distribution or median
#' 
#' \code{distE.mmad} calculates the maximum absolute difference within 
#' two groups and returns the dispersion vector as a result. 
#' @param x A vector of numbers for the first distribution
#' @param y A vector of numbers for the second distribution
#' @param mid Toggles whether to give the distribution or the median
#' @export distE.mmad
#' @import Rfast

# returns a vector of differences between groups
# recycles if x,y are different sizes
distE.mmad <- function(x,y, mid=FALSE){
  if(length(x) < 9 || length(y) < 9){ base::warning('x or y contain less than 10 values')}
  if(is.numeric(x) & is.numeric(y)){
  if(mid == FALSE){
      return( pmax(abs(x - sample(x)), abs(y - sample(y))) ) 
  } else { 
      return( Rfast::med(pmax(abs(x - sample(x)), abs(y - sample(y))) ) )
  }
  } else {
    stop('x and y must be numeric vectors')
  }
}

