#' SHEKEL FUNCTION
#'
#' Dimensions: 4
#' 	The Shekel function has m local minima. Above are the recommended values of m, the ?-vector and the C-matrix; ? is an m-dimensional vector, and C is a 4-by-m-dimensional matrix.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [0, 10], for all i = 1, 2, 3, 4.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, x3, x4)
#' @export shekel
#'

shekel <- function(xx, m=10)
{

  m <- 10
  b <- 0.1 * c(1, 2, 2, 4, 4, 6, 3, 7, 5, 5)
  C <- c(4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
         4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6,
         4.0, 1.0, 8.0, 6.0, 3.0, 2.0, 5.0, 8.0, 6.0, 7.0,
         4.0, 1.0, 8.0, 6.0, 7.0, 9.0, 3.0, 1.0, 2.0, 3.6)
  C <- matrix(C, 4, 10, byrow=TRUE)
  Ct <- t(C)

  xxmat <- matrix(rep(xx,times=m), m, 4, byrow=TRUE)
  inner <- rowSums((xxmat-Ct[,1:4])^2)

  outer <- sum(1/(inner+b))

  y <- -outer
  return(y)
}
