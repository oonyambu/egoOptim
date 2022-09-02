#' HARTMANN 3-DIMENSIONAL FUNCTION
#'
#' Dimensions: 3
#' 	The 3-dimensional Hartmann function has 4 local minima.
#' 	The function is usually evaluated on the hypercube xi <U+2208> (0, 1), for all i = 1, 2, 3.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#' 	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, x3)
#' @export hart3
#'

hart3 <- function(xx)
{
 
 alpha <- c(1.0, 1.2, 3.0, 3.2)
 A <- c(3.0, 10, 30,
 0.1, 10, 35,
 3.0, 10, 30,
 0.1, 10, 35)
 A <- matrix(A, 4, 3, byrow=TRUE)
 P <- 10^(-4) * c(3689, 1170, 2673,
 4699, 4387, 7470,
 1091, 8732, 5547,
 381, 5743, 8828)
 P <- matrix(P, 4, 3, byrow=TRUE)
 xxmat <- matrix(rep(xx,times=4), 4, 3, byrow=TRUE)
 inner <- rowSums(A[,1:3]*(xxmat-P[,1:3])^2)
 outer <- sum(alpha * exp(-inner))
 y <- -outer
 return(y)
}
