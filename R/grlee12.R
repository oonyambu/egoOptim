#' GRAMACY & LEE (2012) FUNCTION
#'
#' Dimensions: 1
#' 	This is a simple one-dimensional test function.
#' 	This function is usually evaluated on the x  [0.5, 2.5].
#' @references \url{https://www.sfu.ca/~ssurjano/grlee12.html}
#' 	Gramacy, R. B., & Lee, H. K. (2012). Cases for the nugget in modeling computer experiments. Statistics and Computing, 22(3), 713-722.
#' 	Ranjan, P. (2013). Comment: EI Criteria for Noisy Computer Simulators. Technometrics, 55(1), 24-28.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @export grlee12
#' @param x a double between 0.5 and 2,5
#'

grlee12 <- function(x)
{

  term1 <- sin(10*pi*x) / (2*x)
  term2 <- (x-1)^4

  y <- term1 + term2
  return(y)
}
