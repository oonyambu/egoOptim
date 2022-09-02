#' CROSS-IN-TRAY FUNCTION
#'
#' Dimensions: 2
#' 	The Cross-in-Tray function has multiple global minima. It is shown here with a smaller domain in the second plot, so that its characteristic "cross" will be visible.
#' 	The function is usually evaluated on the square xi <U+2208> [-10, 10], for all i = 1, 2.
#' 	Reference:
#' 	Test functions for optimization. In Wikipedia. Retrieved June 2013, from https://en.wikipedia.org/wiki/Test_functions_for_optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export crossit
#'

crossit <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  fact1 <- sin(x1)*sin(x2)
  fact2 <- exp(abs(100 - sqrt(x1^2+x2^2)/pi))
	
  y <- -0.0001 * (abs(fact1*fact2)+1)^0.1
  return(y)
}
