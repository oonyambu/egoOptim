#' SIX-HUMP CAMEL FUNCTION
#'
#' Dimensions: 2
#' 	The plot on the left shows the six-hump Camel function on its recommended input domain, and the plot on the right shows only a portion of this domain, to allow for easier viewing of the function's key characteristics. The function has six local minima, two of which are global.
#' 	The function is usually evaluated on the rectangle x1 <U+2208> [-3, 3], x2 <U+2208> [-2, 2].
#' 	Reference:
#' 	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
#' @author Sonja Surjanovic, Simon Fraser University
#'          	Derek Bingham, Simon Fraser University
#' @param xx c(x1, x2)
#' @export camel6
#'

camel6 <- function(xx)
{
  
  x1 <- xx[1]
  x2 <- xx[2]
	
  term1 <- (4-2.1*x1^2+(x1^4)/3) * x1^2
  term2 <- x1*x2
  term3 <- (-4+4*x2^2) * x2^2
	
  y <- term1 + term2 + term3
  return(y)
}
