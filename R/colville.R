#' COLVILLE FUNCTION
#'
#' Dimensions: 4
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-10, 10], for all i = 1, 2, 3, 4.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, x3, x4)
#' @export colville
#'

colville <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
  x3 <- xx[3]
  x4 <- xx[4]
	
  term1 <- 100 * (x1^2-x2)^2
  term2 <- (x1-1)^2
  term3 <- (x3-1)^2
  term4 <- 90 * (x3^2-x4)^2
  term5 <- 10.1 * ((x2-1)^2 + (x4-1)^2)
  term6 <- 19.8*(x2-1)*(x4-1)
	
  y <- term1 + term2 + term3 + term4 + term5 + term6
  return(y)
}
