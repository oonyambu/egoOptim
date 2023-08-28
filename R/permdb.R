#' PERM FUNCTION d, beta
#'
#' Dimensions: d
#' 	The Perm d, ? function.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-d, d], for all i = 1, ?, d.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @param b  = constant (optional), with default value 0.5
#' @export permdb
#'

permdb <- function(xx, b=0.5)
{

  d <- length(xx)
  ii <- c(1:d)
  jj <- matrix(rep(ii,times=d), d, d, byrow=TRUE)
  xxmat <- matrix(rep(xx,times=d), d, d, byrow=TRUE)
  inner <- rowSums((jj^ii+b)*((xxmat/jj)^ii-1))
  outer <- sum(inner^2)

  y <- outer
  return(y)
}
