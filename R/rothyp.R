#' ROTATED HYPER-ELLIPSOID FUNCTION
#'
#' Dimensions: d
#' 	The Rotated Hyper-Ellipsoid function is continuous, convex and unimodal. It is an extension of the Axis Parallel Hyper-Ellipsoid function, also referred to as the Sum Squares function. The plot shows its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-65.536, 65.536], for all i = 1, ?, d.
#' 	Reference:
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export rothyp
#'

rothyp <- function(xx)
{

  d <- length(xx)
  xxmat <- matrix(rep(xx,times=d), d, d, byrow=TRUE)
  xxmatlow <- xxmat
  xxmatlow[upper.tri(xxmatlow)] <- 0

  inner <- rowSums(xxmatlow^2)
  outer <- sum(inner)

  y <- outer
  return(y)
}
