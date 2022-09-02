#' SHUBERT FUNCTION
#'
#' Dimensions: 2
#' 	The Shubert function has several local minima and many global minima. The second plot shows the the function on a smaller input domain, to allow for easier viewing.
#' 	The function is usually evaluated on the square xi <U+2208> [-10, 10], for all i = 1, 2, although this may be restricted to the square xi <U+2208> [-5.12, 5.12], for all i = 1, 2.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export shubert
#'

shubert <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
  ii <- c(1:5)
	
  sum1 <- sum(ii * cos((ii+1)*x1+ii))
  sum2 <- sum(ii * cos((ii+1)*x2+ii))
	
  y <- sum1 * sum2
  return(y)
}
