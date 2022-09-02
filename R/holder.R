#' HOLDER TABLE FUNCTION
#'
#' Dimensions: 2
#' 	The Holder Table function has many local minima, with four global minima.
#' 	The function is usually evaluated on the square xi <U+2208> [-10, 10], for all i = 1, 2.
#' @references \url{https://www.sfu.ca/~ssurjano/holder.html}
#' 	Test functions for optimization. In Wikipedia. Retrieved June 2013, from https://en.wikipedia.org/wiki/Test_functions_for_optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export holder
#'

holder <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  fact1 <- sin(x1)*cos(x2)
  fact2 <- exp(abs(1 - sqrt(x1^2+x2^2)/pi))
	
  y <- -abs(fact1*fact2)
  return(y)
}
