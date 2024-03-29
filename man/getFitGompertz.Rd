% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFitGompertz.R
\name{getFitGompertz}
\alias{getFitGompertz}
\title{Fit a Gompertz function}
\usage{
getFitGompertz(coefs = NULL, id = NULL)
}
\arguments{
\item{coefs}{coefficients obtained by the function \code{\link{readCoefs}}}

\item{id}{'Treatment' name of the sample to be fitted}
}
\description{
This function evaluates the input coefficients from the function \code{\link{readCoefs}} and evaluates the function on a specific range of values (same than the android app)
}
\author{
Mario Fajardo
}

