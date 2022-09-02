#' DE JONG FUNCTION N. 5
#'
#' Dimensions: 2
#' 	The fifth function of De Jong is multimodal, with very sharp drops on a mainly flat surface.
#' 	The function is usually evaluated on the square xi <U+2208> [-65.536, 65.536], for all i = 1, 2.
#' 	Reference:
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export dejong5
#'

dejong5 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  A = matrix(0, 2, 25)
  a <- c(-32, -16, 0, 16, 32)
  A[1,] <- rep(a, times=5)
  A[2,] <- rep(a, each=5)
	
  sumterm1 <- c(1:25)
  sumterm2 <- (x1 - A[1,1:25])^6
  sumterm3 <- (x2 - A[2,1:25])^6
  sum <- sum(1 / (sumterm1+sumterm2+sumterm3))
	
  y <- 1 / (0.002 + sum)
  return(y)
}
