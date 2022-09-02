#' SUM SQUARES FUNCTION
#'
#' Dimensions: d
#' 	The Sum Squares function, also referred to as the Axis Parallel Hyper-Ellipsoid function, has no local minimum except the global one. It is continuous, convex and unimodal. It is shown here in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-10, 10], for all i = 1, ?, d, although this may be restricted to the hypercube xi <U+2208> [-5.12, 5.12], for all i = 1, ?, d.
#' @references \url{https://www.sfu.ca/~ssurjano/sumsqu.html}
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export sumsqu
#'

sumsqu <- function(xx)
{

  ii <- c(1:length(xx))
  sum <- sum(ii*xx^2)

  y <- sum
  return(y)
}
