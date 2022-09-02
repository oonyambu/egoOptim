#' BRANIN FUNCTION, RESCALED
#'
#' Dimensions: 2
#' 	The Branin, or Branin-Hoo, function has three global minima. The recommended values of a, b, c, r, s and t are: a = 1, b = 5.1 / (4p2), c = 5 / p, r = 6, s = 10 and t = 1 / (8p).
#' 	This function is usually evaluated on the square x1 <U+2208> [-5, 10], x2 <U+2208> [0, 15].
#' 	Modifications and Alternate Forms:
#' 	Picheny et al. (2012) use the following rescaled form of the Branin-Hoo function, on [0, 1]2:
#' 	This rescaled form of the function has a mean of zero and a variance of one. The authors also add a small Gaussian error term to the output.
#' 	For the purpose of Kriging prediction, Forrester et al. (2008) use a modified form of the Branin-Hoo function, in which they add a term 5x1 to the response. As a result, there are two local minima and only one global minimum, making it more representative of engineering functions.
#' @references \url{https://www.sfu.ca/~ssurjano/branin.html}
#' 	Dixon, L. C. W., & Szego, G. P. (1978). The global optimization problem: an introduction. Towards global optimization, 2, 1-15.
#' 	Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' 	Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark of kriging-based infill criteria for noisy optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export braninsc
#'

braninsc <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
  
  x1bar <- 15*x1 - 5
  x2bar <- 15 * x2
  
  term1 <- x2bar - 5.1*x1bar^2/(4*pi^2) + 5*x1bar/pi - 6
  term2 <- (10 - 10/(8*pi)) * cos(x1bar)
  
  y <- (term1^2 + term2 - 44.81) / 51.95
  return(y)
}
