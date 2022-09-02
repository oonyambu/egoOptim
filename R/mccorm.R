#' MCCORMICK FUNCTION
#'
#' Dimensions: 2
#' 	The function is usually evaluated on the rectangle x1 <U+2208> [-1.5, 4], x2 <U+2208> [-3, 4].
#' 	Reference:
#' 	Adorio, E. P., & Diliman, U. P. MVF - Multivariate Test Functions Library in C for Unconstrained Global Optimization (2005). Retrieved June 2013, from http://http://www.geocities.ws/eadorio/mvf.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export mccorm
#'

mccorm <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- sin(x1 + x2)
  term2 <-(x1 - x2)^2
  term3 <- -1.5*x1
  term4 <- 2.5*x2
	
  y <- term1 + term2 + term3 + term4 + 1
  return(y)
}
