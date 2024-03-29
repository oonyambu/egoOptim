% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/domain.R
\name{domain}
\alias{domain}
\title{Domain
A function to output the domain of the various functions in the
egoOptim package}
\usage{
domain(fun_name = NULL)
}
\arguments{
\item{fun_name}{An optional character of length 1. The name of the function
of which we desire to obtain the domain of  the dimensions.}
}
\value{
A list with the domain and the global minimum(s):
\item{lower}{A vector containing the lower bounds}
\item{upper}{A vector containing the upper bounds}
\item{opt}{A list containing the global minimum \bold{x*}
and the function value \bold{f(x*)}}
}
\description{
Domain
A function to output the domain of the various functions in the
egoOptim package
}
\examples{
domain()
domain("camel3")
domain()$camel3
}
