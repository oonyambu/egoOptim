#' GOLDSTEIN-PRICE FUNCTION, SCALED
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
#' @export goldprsc
#'

goldprsc <- function(xx)
{
  
  x1bar <- 4*xx[1] - 2
  x2bar <- 4*xx[2] - 2
  fact1a <- (x1bar + x2bar + 1)^2
  fact1b <- 19 - 14*x1bar + 3*x1bar^2 - 14*x2bar + 6*x1bar*x2bar + 3*x2bar^2
  fact1 <- 1 + fact1a*fact1b
  
  fact2a <- (2*x1bar - 3*x2bar)^2
  fact2b <- 18 - 32*x1bar + 12*x1bar^2 + 48*x2bar - 36*x1bar*x2bar + 27*x2bar^2
  fact2 <- 30 + fact2a*fact2b
  
  prod <- fact1*fact2
  
  y <- (log(prod) - 8.693) / 2.427
  return(y)
}
