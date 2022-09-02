#' LEVY FUNCTION
#'
#' Dimensions: d
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-10, 10], for all i = 1, ?, d.
#' 	Reference:
#' 	Laguna, M., & Marti, R. Experimental Testing of Advanced Scatter Search Designs for Global Optimization of Multimodal Functions (2002). Retrieved June 2013, from http://www.uv.es/rmarti/paper/docs/global1.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export levy
#'

levy <- function(xx)
{

  d <- length(xx)
  w <- 1 + (xx - 1)/4

  term1 <- (sin(pi*w[1]))^2
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)

  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))

  y <- term1 + sum + term3
  return(y)
}
