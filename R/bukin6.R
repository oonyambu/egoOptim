#' BUKIN FUNCTION N. 6
#'
#' Dimensions: 2
#' 	The sixth Bukin function has many local minima, all of which lie in a ridge.
#' 	The function is usually evaluated on the rectangle x1 <U+2208> [-15, -5], x2 <U+2208> [-3, 3].
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export bukin6
#'

bukin6 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- 100 * sqrt(abs(x2 - 0.01*x1^2))
  term2 <- 0.01 * abs(x1+10)
	
  y <- term1 + term2
  return(y)
}
