#' Calculate pooled dispersion via MMAD distribution or median
#' 
#' \code{distr.diff} calculates the difference vector of two distributions
#' @param x A vector of numbers for the first distribution
#' @param y A vector of numbers for the second distribution
#' @param mid Toggles whether to give the distribution or the median
distr.diff <- function(x,y, mid=FALSE){
  if(is.numeric(x) & is.numeric(y)){
	if(mid == FALSE){ 
      return(x - y)
    } else {
      return( median( x - y ) )
    }
  }
}