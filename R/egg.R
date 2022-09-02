#' EGGHOLDER FUNCTION
#'
#' Dimensions: 2
#' 	The Eggholder function is a difficult function to optimize, because of the large number of local minima.
#' 	The function is usually evaluated on the square xi <U+2208> [-512, 512], for all i = 1, 2.
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export egg
#'

egg <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- -(x2+47) * sin(sqrt(abs(x2+x1/2+47)))
  term2 <- -x1 * sin(sqrt(abs(x1-(x2+47))))
	
  y <- term1 + term2
  return(y)
}
