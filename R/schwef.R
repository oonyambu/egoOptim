#' SCHWEFEL FUNCTION
#'
#' Dimensions: d
#' 	The Schwefel function is complex, with many local minima. The plot shows the two-dimensional form of the function.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-500, 500], for all i = 1, ?, d.
#' 	Reference:
#' 	GEATbx: Examples of Objective Functions. Retrieved September 2014, from http://www.pg.gda.pl/~mkwies/dyd/geadocu/fcnfun7.html.
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Laguna, M., & Marti, R. Experimental Testing of Advanced Scatter Search Designs for Global Optimization of Multimodal Functions (2002). Retrieved June 2013, from http://www.uv.es/rmarti/paper/docs/global1.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export schwef
#'

schwef <- function(xx)
{

  d <- length(xx)

  sum <- sum(xx*sin(sqrt(abs(xx))))
  y <- 418.9829*d - sum
  return(y)
}
