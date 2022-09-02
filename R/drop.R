#' DROP-WAVE FUNCTION
#'
#' Dimensions: 2
#' 	The Drop-Wave function is multimodal and highly complex. The second plot above shows the function on a smaller input domain, to illustrate its characteristic features.
#' 	The function is usually evaluated on the square xi <U+2208> [-5.12, 5.12], for all i = 1, 2.
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export drop
#'

drop <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  frac1 <- 1 + cos(12*sqrt(x1^2+x2^2))
  frac2 <- 0.5*(x1^2+x2^2) + 2
	
  y <- -frac1/frac2
  return(y)
}
