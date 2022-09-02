#' POWELL FUNCTION
#'
#' Dimensions: d
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-4, 5], for all i = 1, ?, d.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Laguna, M., & Marti, R. Experimental Testing of Advanced Scatter Search Designs for Global Optimization of Multimodal Functions (2002). Retrieved June 2013, from http://www.uv.es/rmarti/paper/docs/global1.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export powell
#'

powell <- function(xx)
{

  d <- length(xx)

  xxa <- xx[seq(1, d-3, 4)]
  xxb <- xx[seq(2, d-2, 4)]
  xxc <- xx[seq(3, d-1, 4)]
  xxd <- xx[seq(4, d, 4)]
  sumterm1 <- (xxa + 10*xxb)^2
  sumterm2 <- 5 * (xxc - xxd)^2
  sumterm3 <- (xxb - 2*xxc)^4
  sumterm4 <- 10 * (xxa - xxd)^4
  sum <- sum(sumterm1 + sumterm2 + sumterm3 + sumterm4)

  y <- sum
  return(y)
}
