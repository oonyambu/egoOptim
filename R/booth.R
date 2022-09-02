#' BOOTH FUNCTION
#'
#' Dimensions: 2
#' 	The function is usually evaluated on the square xi <U+2208> [-10, 10], for all i = 1, 2.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export booth
#'

booth <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- (x1 + 2*x2 - 7)^2
  term2 <- (2*x1 + x2 - 5)^2
	
  y <- term1 + term2
  return(y)
}
