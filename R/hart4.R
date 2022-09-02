#' HARTMANN 4-DIMENSIONAL FUNCTION
#'
#' Dimensions: 4
#' 	The 4-dimensional Hartmann function is multimodal. It is given here in the form of Picheny et al. (2012), having a mean of zero and a variance of one. The authors also add a small Gaussian error term to the output.
#' 	The function is evaluated on the hypercube xi <U+2208> [0, 1], for all i = 1, 2, 3, 4.
#' @references \url{https://www.sfu.ca/~ssurjano/hart4.html}
#' 	Dixon, L. C. W., & Szego, G. P. (1978). The global optimization problem: an introduction. Towards global optimization, 2, 1-15.
#' 	Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark of kriging-based infill criteria for noisy optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, x3, x4)
#' @export hart4
#'

hart4 <- function(xx)
{
  
  alpha <- c(1.0, 1.2, 3.0, 3.2)
  A <- c(10, 3, 17, 3.5, 1.7, 8,
         0.05, 10, 17, 0.1, 8, 14,
         3, 3.5, 1.7, 10, 17, 8,
         17, 8, 0.05, 10, 0.1, 14)
  A <- matrix(A, 4, 6, byrow=TRUE)
  P <- 10^(-4) * c(1312, 1696, 5569, 124, 8283, 5886,
                   2329, 4135, 8307, 3736, 1004, 9991,
                   2348, 1451, 3522, 2883, 3047, 6650,
                   4047, 8828, 8732, 5743, 1091, 381)
  P <- matrix(P, 4, 6, byrow=TRUE)
  
  xxmat <- matrix(rep(xx,times=4), 4, 4, byrow=TRUE)
  inner <- rowSums(A[,1:4]*(xxmat-P[,1:4])^2)
  outer <- sum(alpha * exp(-inner))
  
  y <- (1.1 - outer) / 0.839
  return(y)
}
