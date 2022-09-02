#' BEALE FUNCTION
#'
#' Dimensions: 2
#' 	The Beale function is multimodal, with sharp peaks at the corners of the input domain.
#' 	The function is usually evaluated on the square xi <U+2208> [-4.5, 4.5], for all i = 1, 2.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export beale
#'

beale <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- (1.5 - x1 + x1*x2)^2
  term2 <- (2.25 - x1 + x1*x2^2)^2
  term3 <- (2.625 - x1 + x1*x2^3)^2
	
  y <- term1 + term2 + term3
  return(y)
}
