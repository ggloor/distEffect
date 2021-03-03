# returns a vector of differences between groups
# recycles if x,y are different sizes
mmad.effect <- function(x,y){
  return( pmax(abs(x - sample(x)), abs(y - sample(y))) ) 
}

# returns a vector of standardized effect sizes
# recycles if x,y are different sizes
dist.effect <- function(x,y, mid=FALSE){

  if(mid == TRUE){ 
      return(median ((x - y)/mmad.effect(x,y) ) )
  } else {
      return( (x - y)/mmad.effect(x,y) )
  }
}

# returns the overlap of the two distributions
# calculated from effect size vector
ol.effect <- function(x){
	e.size <- c(sum(x > 0), sum(x < 0)) + 0.5
	l.size <- log(e.size) - mean(log(e.size))
	p.size <- exp(l.size - pmax(l.size))
	return(min(p.size/sum(p.size)))
}

