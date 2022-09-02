#' FORRESTER ET AL. (2008) FUNCTION
#'
#' Dimensions: 1
#' 	This function is a simple one-dimensional test function. It is multimodal, with one global minimum, one local minimum and a zero-gradient inflection point.
#' 	The function is evaluated on x <U+2208> [0, 1].
#' 	Modifications and Alternative Forms:
#' 	For the purpose of multi-fidelity analysis, the following function is used for the lower fidelity code:
#' 	Here, the constants A, B and C can be varied to improve the fidelity of the low-fidelity function. In the plot above, the values used are (as in Forrester et al., 2008): A = 0.5, B = 10, C = -5.
#' 	Reference:
#' 	Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @export forretal08
#'

forretal08 <- function(x)
{
  
  fact1 <- (6*x - 2)^2
  fact2 <- sin(12*x - 4)
  
  y <- fact1 * fact2
  return(y)
}
