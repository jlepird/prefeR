% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interface.R
\name{Flat}
\alias{Flat}
\title{A convenience function for generating a flat prior.}
\usage{
Flat()
}
\value{
The zero function.
}
\description{
A convenience function for generating a flat prior.
}
\examples{
Flat()(1) == 0.0
}
\seealso{
Other priors: 
\code{\link{Exp}()},
\code{\link{Normal}()}
}
\concept{priors}
