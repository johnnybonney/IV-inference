% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_callcheck.R
\name{get_xz}
\alias{get_xz}
\title{Auxiliary function: extract X and Z covariates from a formula}
\usage{
get_xz(fm, inst = FALSE, terms = FALSE)
}
\arguments{
\item{fm}{the formula.}

\item{inst}{boolean expression, set to TRUE if the instrument names
are to be extracted. Otherwise, the covariate names are
extracted.}

\item{terms}{boolean expression, set to TRUE if the terms in the
formula \code{fm} should be returned instead of the variable
names.}
}
\description{
Auxiliary function that takes in a formula, and extracts the
variable names of either the covariates or instruments.
}
