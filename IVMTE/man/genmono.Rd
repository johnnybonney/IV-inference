% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mtr.R
\name{genmono}
\alias{genmono}
\title{Generating monomials}
\usage{
genmono(vector, basis, zero = FALSE, as.function = FALSE)
}
\arguments{
\item{vector}{vector of polynomial coefficients.}

\item{basis}{vector of exponents corresponding to each element of
\code{vector}.}

\item{zero}{logical, if \code{FALSE} then \code{vector} does not
include an element for the constant term. The vector
\code{basis} will need to be adjusted to account for this in
order to generate the correct polynomial and monomials.}

\item{as.function}{boolean, if \code{FALSE} then polynomials are
returned; if \code{TRUE} then a function corresponding to the
polynomial is returned.}
}
\value{
A list of monomials, in the form of the \code{polynom}
    package.
}
\description{
This function takes in a first vector of coefficients, and a second
vector declaring which univariate polynomial basis corresponds to
each element of the first vector. Then it generates a list of
monomials corresponding to the polynomial.
}
