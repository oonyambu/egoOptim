% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/langer.R
\name{langer}
\alias{langer}
\title{LANGERMANN FUNCTION}
\usage{
langer(xx, m = 5, cvec, A)
}
\arguments{
\item{xx}{= c(x1, x2, ..., xd)}

\item{m}{= constant (optional), with default value 5}

\item{cvec}{m-dimensional vector (optional), with default value c(1, 2, 5, 2, 3)}

\item{A}{= (mxd)-dimensional matrix (optional), with default value:}
}
\description{
Dimensions: d
	The Langermann function is multimodal, with many unevenly distributed local minima. The recommended values of m, c and A, as given by Molga & Smutnicki (2005) are (for d = 2): m = 5, c = (1, 2, 5, 2, 3) and:
	The function is usually evaluated on the hypercube xi <U+2208> [0, 10], for all i = 1, ?, d.
	Reference:
	Adorio, E. P., & Diliman, U. P. MVF - Multivariate Test Functions Library in C for Unconstrained Global Optimization (2005). Retrieved June 2013, from http://http://www.geocities.ws/eadorio/mvf.pdf.
	Molga, M., & Smutnicki, C. Test functions for optimization needs (2005). Retrieved June 2013, from http://www.zsd.ict.pwr.wroc.pl/files/docs/functions.pdf.
}
\author{
Sonja Surjanovic, Simon Fraser University
         	Derek Bingham, Simon Fraser University
}
