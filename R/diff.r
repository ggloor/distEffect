#' Calculate pooled dispersion via MMAD distribution or median
#' 
#' \code{distE.diff} calculates the difference vector of two distributions
#' @param x A vector of numbers for the first distribution
#' @param y A vector of numbers for the second distribution
#' @param mid Toggles whether to give the distribution or the median
#' @export distE.diff
#' @import stats

distE.diff <- function(x,y, mid=FALSE){
  if(is.numeric(x) & is.numeric(y)){
	if(mid == FALSE){ 
      return(x - y)
    } else {
      return( stats::median( x - y ) )
    }
  }
}