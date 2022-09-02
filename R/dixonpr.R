#' DIXON-PRICE FUNCTION
#'
#' Dimensions: d
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-10, 10], for all i = 1, ?, d.
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export dixonpr
#'

dixonpr <- function(xx)
{

  x1 <- xx[1]
  d <- length(xx)
  term1 <- (x1-1)^2

  ii <- c(2:d)
  xi <- xx[2:d]
  xold <- xx[1:(d-1)]
  sum <- sum(ii * (2*xi^2 - xold)^2)

  y <- term1 + sum
  return(y)
}
