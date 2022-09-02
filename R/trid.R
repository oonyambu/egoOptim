#' TRID FUNCTION
#'
#' Dimensions: d
#' 	The Trid function has no local minimum except the global one. It is shown here in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-d2, d2], for all i = 1, ?, d.
#' @references \url{https://www.sfu.ca/~ssurjano/trid.html}
#' 	Adorio, E. P., & Diliman, U. P. MVF - Multivariate Test Functions Library in C for Unconstrained Global Optimization (2005). Retrieved August 2017, from http://www.geocities.ws/eadorio/mvf.pdf.
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export trid
#'

trid <- function(xx)
{

  d <- length(xx)
  xi <- xx[2:d]
  xold <- xx[1:(d-1)]

  sum1 = (xx[1]-1)^2 + sum((xi-1)^2)
  sum2 <- sum(xi*xold)

  y <- sum1 - sum2
  return(y)
}
