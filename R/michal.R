#' MICHALEWICZ FUNCTION
#'
#' Dimensions: d
#' 	The Michalewicz function has d! local minima, and it is multimodal. The parameter m defines the steepness of they valleys and ridges; a larger m leads to a more difficult search. The recommended value of m is m = 10. The function's two-dimensional form is shown in the plot above.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [0, p], for all i = 1, ?, d.
#' @references \url{https://www.sfu.ca/~ssurjano/michal.html}
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @param m constant (optional), with default value 10
#' @export michal
#'

michal <- function(xx, m=10)
{

  ii <- c(1:length(xx))
  sum <- sum(sin(xx) * (sin(ii*xx^2/pi))^(2*m))

  y <- -sum
  return(y)
}
