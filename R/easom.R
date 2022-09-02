#' EASOM FUNCTION
#'
#' Dimensions: 2
#' 	The Easom function has several local minima. It is unimodal, and the global minimum has a small area relative to the search space.
#' 	The function is usually evaluated on the square xi <U+2208> [-100, 100], for all i = 1, 2.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export easom
#'

easom <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  fact1 <- -cos(x1)*cos(x2)
  fact2 <- exp(-(x1-pi)^2-(x2-pi)^2)
	
  y <- fact1*fact2
  return(y)
}
