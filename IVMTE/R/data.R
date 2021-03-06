#' Mosquito data set
#'
#' A simulated population-level data set characterizing the effect of
#' purchasing mosquito nets, and the likelihood of contracting
#' malaria. A randomly assigned subsidy to purchase mosquito nets was
#' provided as part of an experiment.
#'
#' @format A data frame with 400 rows and 7 columns.
#' \describe{
#'   \item{i}{index for observation}
#'   \item{z}{categorical variable for level of subsidy}
#'   \item{pz}{probability of purchasing mosquito net}
#'   \item{d}{indicator for whether or not mosquito net was purchased}
#' 
#'   \item{ey0}{counterfactual probability of contracting malaria
#'   conditional on not purchasing a mosquito net.}
#'
#'   \item{ey1}{counterfactual probability of contracting malaria
#'   conditional on purchasing a mosquito net.}
#' 
#'   \item{ey}{the observed probability of contracting malaria.}
#' }
#' @source Simulated, based on Mogstad, Torgovitsky (2017).
"dtm"

#' Covariates data set
#'
#' A simulated population-level data set characterizing the effect of
#' a treatment on an outcome. The data includes a treatment indicator,
#' two covariates and two instruments.
#'
#' @format A data frame with 10,000 rows and 14 columns.
#' \describe{
#'   \item{x1}{covariate 1}
#'   \item{x2}{covariate 2}
#'   \item{z1}{instrument 1}
#'   \item{z2}{instrument 2}
#'   \item{latent}{latent variable determining treatment participation}
#'   \item{p}{probability of treatment uptake}
#'   \item{ey0}{counterfactual outcome when not a recipient of treatment}
#'   \item{ey1}{counterfactual outcome when a recipient of treatment}
#'   \item{f}{density}
#'   \item{multiplier}{number of observations in data set}
#'   \item{d}{indicator for treatment (d = 1) versus control (d = 0) group}
#'   \item{i}{counter of observations within support of (X, Z)}
#'   \item{dcut}{the number of treated subjects by (X, Z)}
#'   \item{ey}{the observed outcome}
#' }
#' @source Simulated.
"dtcf"

#' Covariates data set distribution
#'
#' The distribution from which \code{dtcf} was generated.
#'
#' @format A data frame with 36 rows and 10 columns.
#' \describe{
#'   \item{x1}{covariate 1}
#'   \item{x2}{covariate 2}
#'   \item{z1}{instrument 1}
#'   \item{z2}{instrument 2}
#'   \item{latent}{latent variable determining treatment participation}
#'   \item{p}{probability of treatment uptake}
#'   \item{ey0}{counterfactual outcome when not a recipient of treatment}
#'   \item{ey1}{counterfactual outcome when a recipient of treatment}
#'   \item{f}{density}
#'   \item{multiplier}{number of observations required in data set}
#' }
#' @source Simulated.
"dtc"

#' Splines data set
#'
#' A simulated population-level data set characterizing the effect of
#' a treatment on an outcome. The data includes a treatment indicator,
#' a single covariate, and a single covariate. The unobservable terms
#' generating the outcomes are generated according to the following
#' specifications:
#
#' y1 ~ beta0 + beta1 * x + uSpline(degree = 2,
#'                                 knots  = c(0.3, 0.6),
#'                                 intercept = FALSE)
#'
#' y0 = x : uSpline(degree = 0,
#'                  knots  = c(0.2, 0.5, 0.8),
#'                  intercept = TRUE)
#'      + uSpline(degree = 1,
#'                knots  = c(0.4),
#'                intercept = TRUE)
#'      + beta3 * I(u ^ 2)
#'
#' @format A data frame with 4,200 rows and 8 columns.
#' \describe{
#'   \item{x}{covariate}
#'   \item{z}{instrument}
#'   \item{f}{density}
#'   \item{p}{probability of treatment uptake}
#'   \item{ey1}{counterfactual outcome when a recipient of treatment}
#'   \item{ey0}{counterfactual outcome when not a recipient of treatment}
#'   \item{d}{indicator for treatment (d = 1) versus control (d = 0) group}
#'   \item{ey}{the observed outcome}
#' }
#' @source Simulated.
"dtsf"

#' Splines data set distribution
#'
#' The distribution from which \code{dtsf} was generated.
#'
#' @format A data frame with 6 rows and 9 columns.
#' \describe{
#'   \item{group}{indicates combination of (X, Z)}
#'   \item{x}{covariate}
#'   \item{z}{instrument}
#'   \item{f}{density}
#'   \item{p}{probability of treatment uptake}
#'   \item{ey1}{counterfactual outcome when a recipient of treatment}
#'   \item{ey0}{counterfactual outcome when not a recipient of treatment}
#'   \item{multiplier}{number of observations in the data set}
#'   \item{controls}{number of controls in the data set}
#' }
#' @source Simulated.
"dts"
