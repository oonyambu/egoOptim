#' LEVY FUNCTION N. 13
#'
#' Dimensions: 2
#' 	The function is usually evaluated on the square xi <U+2208> [-10, 10], for all i = 1, 2.
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export levy13
#'

levy13 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- (sin(3*pi*x1))^2
  term2 <- (x1-1)^2 * (1+(sin(3*pi*x2))^2)
  term3 <- (x2-1)^2 * (1+(sin(2*pi*x2))^2)
	
  y <- term1 + term2 + term3
  return(y)
}
