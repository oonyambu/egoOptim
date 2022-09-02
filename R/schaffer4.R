#' SCHAFFER FUNCTION N. 4
#'
#' Dimensions: 2
#' 	The fourth Schaffer function. It is shown on a smaller input domain in the second plot to show detail.
#' 	The function is usually evaluated on the square xi <U+2208> [-100, 100], for all i = 1, 2.
#' 	Reference:
#' 	Test functions for optimization. In Wikipedia. Retrieved June 2013, from https://en.wikipedia.org/wiki/Test_functions_for_optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export schaffer4
#'

schaffer4 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  fact1 <- (cos(sin(abs(x1^2-x2^2))))^2 - 0.5
  fact2 <- (1 + 0.001*(x1^2+x2^2))^2
	
  y <- 0.5 + fact1/fact2
  return(y)
}
