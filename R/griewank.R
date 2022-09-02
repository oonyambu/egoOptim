#' GRIEWANK FUNCTION
#'
#' Dimensions: d
#' 	The Griewank function has many widespread local minima, which are regularly distributed. The complexity is shown in the zoomed-in plots.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-600, 600], for all i = 1, ?, d.
#' @references \url{https://www.sfu.ca/~ssurjano/griewank.html}
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export griewank
#'

griewank <- function(xx)
{

  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))

  y <- sum - prod + 1
  return(y)
}
