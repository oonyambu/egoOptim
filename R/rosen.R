#' ROSENBROCK FUNCTION
#'
#' Dimensions: d
#' 	The Rosenbrock function, also referred to as the Valley or Banana function, is a popular test problem for gradient-based optimization algorithms. It is shown in the plot above in its two-dimensional form.
#' 	The function is unimodal, and the global minimum lies in a narrow, parabolic valley. However, even though this valley is easy to find, convergence to the minimum is difficult (Picheny et al., 2012).
#' 	The function is usually evaluated on the hypercube xi <U+2208> [-5, 10], for all i = 1, ?, d, although it may be restricted to the hypercube xi <U+2208> [-2.048, 2.048], for all i = 1, ?, d.
#' 	Modifications and Alternate Forms:
#' 	Picheny et al. (2012) use the following rescaled form of the function, with d = 4, on [0, 1]2:
#' 	This rescaled form of the function has a mean of zero and a variance of one. The authors also add a small Gaussian error term to the output.
#' @references \url{https://www.sfu.ca/~ssurjano/rosen.html}
#' 	Dixon, L. C. W., & Szego, G. P. (1978). The global optimization problem: an introduction. Towards global optimization, 2, 1-15.
#' 	http://www-optima.amp.i.kyoto-u.ac.jp/member/student/hedar/Hedar_files/TestGO.htm.
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' 	Picheny, V., Wagner, T., & Ginsbourger, D. (2012). A benchmark of kriging-based infill criteria for noisy optimization.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2, ..., xd)
#' @export rosen
#'

rosen <- function(xx)
{

  d <- length(xx)
  xi <- xx[1:(d-1)]
  xnext <- xx[2:d]

  sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)

  y <- sum
  return(y)
}
