% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forretal08lc.R
\name{forretal08lc}
\alias{forretal08lc}
\title{FORRESTER ET AL. (2008) FUNCTION, LOWER FIDELITY CODE}
\usage{
forretal08lc(x, A = 0.5, B = 10, C = -5)
}
\arguments{
\item{x}{scalar}

\item{A}{constant (optional), with default value 0.5}

\item{B}{constant (optional), with default value 10}

\item{C}{constant (optional), with default value -5}
}
\description{
Dimensions: 1
	This function is a simple one-dimensional test function. It is multimodal, with one global minimum, one local minimum and a zero-gradient inflection point.
	The function is evaluated on x <U+2208> [0, 1].
	Modifications and Alternative Forms:
	For the purpose of multi-fidelity analysis, the following function is used for the lower fidelity code:
	Here, the constants A, B and C can be varied to improve the fidelity of the low-fidelity function. In the plot above, the values used are (as in Forrester et al., 2008): A = 0.5, B = 10, C = -5.
	Reference:
	Forrester, A., Sobester, A., & Keane, A. (2008). Engineering design via surrogate modelling: a practical guide. Wiley.
	Calls: forretal08.r
	This function is used as the "low-accuracy code" version of the function
	forretal08.r.
}
\author{
Sonja Surjanovic, Simon Fraser University
         	Derek Bingham, Simon Fraser University
}
