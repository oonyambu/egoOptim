#' SUM OF DIFFERENT POWERS FUNCTION
#'
#' Dimensions: d
#' 	The Sum of Different Powers function is unimodal. It is shown here in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-1, 1], for all i = 1, ?, d.
#' 	Reference:
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export sumpow
#'

sumpow <- function(xx)
{

  ii <- c(1:length(xx))
  sum <- sum((abs(xx))^(ii+1))

  y <- sum
  return(y)
}
