#' RASTRIGIN FUNCTION
#'
#' Dimensions: d
#' 	The Rastrigin function has several local minima. It is highly multimodal, but locations of the minima are regularly distributed. It is shown in the plot above in its two-dimensional form.
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-5.12, 5.12], for all i = 1, ?, d.
#' @references \url{https://www.sfu.ca/~ssurjano/rastr.html}
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Pohlheim, H. GEATbx Examples: Examples of Objective Functions (2005). Retrieved June 2013, from http://www.geatbx.com/download/GEATbx_ObjFunExpl_v37.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export rastr
#'

rastr <- function(xx)
{

  d <- length(xx)

  sum <- sum(xx^2 - 10*cos(2*pi*xx))

  y <- 10*d + sum
  return(y)
}
