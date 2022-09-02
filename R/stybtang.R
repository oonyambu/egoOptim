#' STYBLINSKI-TANG FUNCTION
#'
#' Dimensions: d
#' 	The Styblinski-Tang function is shown here in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-5, 5], for all i = 1, ?, d.
#' 	Reference:
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export stybtang
#'

stybtang <- function(xx)
{

  sum <- sum(xx^4 - 16*xx^2 + 5*xx)

  y <- sum/2
  return(y)
}
