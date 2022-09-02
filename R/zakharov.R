#' ZAKHAROV FUNCTION
#'
#' Dimensions: d
#' 	The Zakharov function has no local minima except the global one. It is shown here in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-5, 10], for all i = 1, ?, d.
#' 	Reference:
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export zakharov
#'

zakharov <- function(xx)
{

  ii <- c(1:length(xx))
  sum1 <- sum(xx^2)
  sum2 <- sum(0.5*ii*xx)

  y <- sum1 + sum2^2 + sum2^4
  return(y)
}
