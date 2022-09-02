#' THREE-HUMP CAMEL FUNCTION
#'
#' Dimensions: 2
#' 	The plot on the left shows the three-hump Camel function on its recommended input domain, and the plot on the right shows only a portion of this domain, to allow for easier viewing of the function's key characteristics. The function has three local minima.
#' 	The function is usually evaluated on the square xi <U+2208> [-5, 5], for all i = 1, 2.
#' 	Reference:
#' 	Test functions for optimization. In Wikipedia. Retrieved June 2013, from https://en.wikipedia.org/wiki/Test_functions_for_optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export camel3
#'

camel3 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- 2*x1^2
  term2 <- -1.05*x1^4
  term3 <- x1^6 / 6
  term4 <- x1*x2
  term5 <- x2^2
	
  y <- term1 + term2 + term3 + term4 + term5
  return(y)
}
