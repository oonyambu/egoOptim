#' GOLDSTEIN-PRICE FUNCTION
#'
#' Dimensions: 2
#' 	The Goldstein-Price function has several local minima.
#' 	The function is usually evaluated on the square xi <U+2208> [-2, 2], for all i = 1, 2.
#' 	Modifications and Alternate Forms:
#' 	Picheny et al. (2012) use the following logarithmic form of the Goldstein-Price function, on [0, 1]2:
#' 	This rescaled form of the function has a mean of zero and a variance of one. The authors also add a small Gaussian error term to the output.
#' @references \url{https://www.sfu.ca/~ssurjano/goldpr.html}
#' 	Dixon, L. C. W., & Szego, G. P. (1978). The global optimization problem: an introduction. Towards global optimization, 2, 1-15.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' 	Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark of kriging-based infill criteria for noisy optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export goldpr
#'

goldpr <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  fact1a <- (x1 + x2 + 1)^2
  fact1b <- 19 - 14*x1 + 3*x1^2 - 14*x2 + 6*x1*x2 + 3*x2^2
  fact1 <- 1 + fact1a*fact1b
	
  fact2a <- (2*x1 - 3*x2)^2
  fact2b <- 18 - 32*x1 + 12*x1^2 + 48*x2 - 36*x1*x2 + 27*x2^2
  fact2 <- 30 + fact2a*fact2b
	
  y <- fact1*fact2
  return(y)
}
