#' POWER SUM FUNCTION
#'
#' Dimensions: d
#' 	The Power Sum function. It is shown above in its two-dimensional form. The recommended value of the b-vector, for d = 4, is: b = (8, 18, 44, 114).
#' 	The function is usually evaluated on the hypercube xi <U+2208> [0, d], for all i = 1, ?, d.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' 	b  = d-dimensional vector (optional), with default value
#'      	c(8, 18, 44, 114) (when d=4)
#' @export powersum
#'

powersum <- function(xx, b)
{

  d <- length(xx)
  ii <- c(1:d)

  if (missing(b)) {
    if (d == 4){
      b <- c(8, 18, 44, 114)
    }
    else {
      stop('Value of the d-dimensional vector b is required.')
    }
  }

  xxmat <- matrix(rep(xx,times=d), d, d, byrow=TRUE)
  inner <- rowSums(xxmat^ii)
  outer <- sum((inner-b)^2)

  y <- outer
  return(y)
}
