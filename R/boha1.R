#' BOHACHEVSKY FUNCTION 1
#'
#' Dimensions: 2
#' 	The Bohachevsky functions all have the same similar bowl shape. The one shown above is the first function.
#' 	The functions are usually evaluated on the square xi <U+2208> [-100, 100], for all i = 1, 2.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export boha1
#'

boha1 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- x1^2
  term2 <- 2*x2^2
  term3 <- -0.3 * cos(3*pi*x1)
  term4 <- -0.4 * cos(4*pi*x2)
	
  y <- term1 + term2 + term3 + term4 + 0.7
  return(y)
}
