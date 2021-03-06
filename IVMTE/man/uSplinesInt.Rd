% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mtr.R
\name{uSplinesInt}
\alias{uSplinesInt}
\title{Integrated splines}
\usage{
uSplinesInt(x, knots, degree = 0, intercept = TRUE)
}
\arguments{
\item{x}{the points to evaluate the integral of the the splines.}

\item{knots}{the knots of the spline.}

\item{degree}{the degree of the spline; default is set to 0
(constant splines).}

\item{intercept}{boolean, set to TRUE if intercept term is to be
included (i.e. an additional basis such that the sum of the
splines at every point in \code{x} is equal to 1).}
}
\value{
a matrix, the values of the integrated splines. Each row
    corresponds to a value of \code{x}; each column corresponds to
    a basis defined by the degrees and knots.
}
\description{
This function integrates out splines that the user specifies when
declaring the MTRs. This is to be used when generating the gamma
moments.
}
\examples{
\dontrun{
Since the splines are declared as part of the MTR, you will need
to have parsed out the spline command. Thus, this command will be
called via eval(parse(text = .)). In the examples below, the
commands are parsed from the object \\code{splineslist} generated
by \\code{\\link[MST]{removeSplines}}. The names of the elements in
the list are the spline commands, and the elements themselves are
the terms that interact with the splines.

eval(parse(text = gsub("uSplines\\\\(",
                       "uSplinesInt(x = x, ",
                       names(splineslist)[1])))

eval(parse(text = gsub("uSplines\\\\(",
                       "uSplinesInt(x = x, ",
                        names(splineslist)[2])))
}
}
