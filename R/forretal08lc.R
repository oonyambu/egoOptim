#' FORRESTER ET AL. (2008) FUNCTION, LOWER FIDELITY CODE
#'
#' Dimensions: 1
#' 	This function is a simple one-dimensional test function. It is multimodal, with one global minimum, one local minimum and a zero-gradient inflection point.
#' 	The function is evaluated on x <U+2208> [0, 1].
#' 	Modifications and Alternative Forms:
#' 	For the purpose of multi-fidelity analysis, the following function is used for the lower fidelity code:
#' 	Here, the constants A, B and C can be varied to improve the fidelity of the low-fidelity function. In the plot above, the values used are (as in Forrester et al., 2008): A = 0.5, B = 10, C = -5.
#' 	Reference:
#' 	Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
#' 	Calls: forretal08.r
#' 	This function is used as the "low-accuracy code" version of the function
#' 	forretal08.r.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param x scalar
#' @param A constant (optional), with default value 0.5
#' @param B constant (optional), with default value 10
#' @param C constant (optional), with default value -5
#' @export forretal08lc
#'

forretal08lc <- function(x, A=0.5, B=10, C=-5)
{
  
  source('forretal08.r')
  yh <- forretal08(x)
  
  term1 <- A * yh
  term2 <- B * (x-0.5)
  
  y <- term1 + term2 - C
  return(y)
}
