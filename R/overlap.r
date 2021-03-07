#' Calculate pooled dispersion via MMAD distribution or median
#' 
#' \code{distr.ol} calculates the overlap of distributions given the effect size vector
#' @param x A vector of effect sizes
#' @param mid Toggles whether to give the distribution or the median

# returns the overlap of the two distributions
# calculated from effect size vector
# note that this does not use the Aitchison mean function
distr.ol <- function(x){
	e.size <- c(sum(x > 0), sum(x < 0)) + 0.5
	l.size <- log(e.size) - mean(log(e.size))
	p.size <- exp(l.size - pmax(l.size))
	return(min(p.size/sum(p.size)))
}

