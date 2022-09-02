#' LANGERMANN FUNCTION
#'
#' Dimensions: d
#' 	The Langermann function is multimodal, with many unevenly distributed local minima. The recommended values of m, c and A, as given by Molga & Smutnicki (2005) are (for d = 2): m = 5, c = (1, 2, 5, 2, 3) and:
#' 	The function is usually evaluated on the hypercube xi <U+2208> [0, 10], for all i = 1, ?, d.
#' 	Reference:
#' 	Adorio, E. P., & Diliman, U. P. MVF - Multivariate Test Functions Library in C for Unconstrained Global Optimization (2005). Retrieved June 2013, from http://http://www.geocities.ws/eadorio/mvf.pdf.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' 	xx   = c(x1, x2, ..., xd)
#' 	m    = constant (optional), with default value 5
#' @param cvec m-dimensional vector (optional), with default value c(1, 2, 5, 2, 3)
#' 	A    = (mxd)-dimensional matrix (optional), with default value:
#' @export langer
#'

langer <- function(xx, m=5, cvec, A)
{

  d <- length(xx)

  if (missing(cvec)) {
    if (m == 5){
      cvec <- c(1,2,5,2,3)
    }
    else {
      stop('Value of the m-dimensional vector cvec is required.')
    }
  }

  if (missing(A)) {
    if (m==5 && d==2) {
      A <- matrix(c(3,5,5,2,2,1,1,4,7,9),5,2,byrow=TRUE)
    }
    else {
        stop('Value of the (mxd)-dimensional matrix A is required.')
    }
  }

  xxmat <- matrix(rep(xx,times=m), m, d, byrow=TRUE)
  inner <- rowSums((xxmat-A[,1:d])^2)
  outer <- sum(cvec * exp(-inner/pi) * cos(pi*inner))

  y <- outer
  return(y)
}
