#' Auxiliary function: \code{which} for lists
#'
#' Auxiliary function that makes it possible to use \code{which} with
#' a list.
#' @param vector the vector for which we want to check the entries of
#' @param obj the value for which we want the vector to match on.
#' @return a vector of positions where the elements in \code{vector}
#'     are equal to \code{obj}.
whichforlist <- function(vector, obj) {
    which(vector == obj)
}

#' Auxiliary function: extracting elements from strings
#'
#' This auxiliary function extracts the (string) element in the
#' \code{position} argument of the \code{vector} argument.
#' @param vector the vector from which we want to extract the
#'     elements.
#' @param position the position in \code{vector} to extract.
#' @param truncation the number of characters from the front of the
#'     element being extracted that should be dropped.
#' @return A chracter/string.
vecextract <- function(vector, position, truncation = 0) {
    elem <- vector[position]
    elem <- substr(elem, truncation + 1, nchar(elem))
    return(elem)
}

#' Generating monomials
#'
#' This function takes in a first vector of coefficients, and a second
#' vector declaring which univariate polynomial basis corresponds to
#' each element of the first vector. Then it generates a list of
#' monomials corresponding to the polynomial.
#' @param vector vector of polynomial coefficients.
#' @param basis vector of exponents corresponding to each element of
#'     \code{vector}.
#' @param zero logical, if \code{FALSE} then \code{vector} does not
#'     include an element for the constant term. The vector
#'     \code{basis} will need to be adjusted to account for this in
#'     order to generate the correct polynomial and monomials.
#' @param as.function boolean, if \code{FALSE} then polynomials are
#'     returned; if \code{TRUE} then a function corresponding to the
#'     polynomial is returned.
#' @return A list of monomials, in the form of the \code{polynom}
#'     package.
genmono <- function(vector, basis, zero = FALSE, as.function = FALSE) {

    if (!zero) basis <- basis + 1

    monolist  <- mapply(genej, pos = basis, length = basis, SIMPLIFY = FALSE)
    polyinput <- mapply("*", monolist, vector, SIMPLIFY = FALSE)

    if (as.function == FALSE) {
        poly <- lapply(polyinput, polynom::polynomial)
    } else {
        poly <- lapply(polyinput,
                       function(x) as.function(polynom::polynomial(x)))
    }
    return(poly)
}

#' Generating polynomial functions
#'
#' This function takes in a first vector of coefficients, and a second
#' vector declaring which univariate polynomial basis corresponds to
#' each element of the first vector. Then it generates a polynomial
#' function.
#' @param vector vector of polynomial coefficients.
#' @param basis vector of exponents corresponding to each element of
#'     \code{vector}.
#' @param zero logical, if \code{FALSE} then \code{vector} does not
#'     include an element for the constant term. The vector
#'     \code{basis} will need to be adjusted to account for this in
#'     order to generate the correct polynomial and monomials.
#' @return A function in the form of the \code{polynom}
#'     package.
genpoly <- function(vector, basis, zero = FALSE) {
    if (!zero) basis <- basis + 1
    polyvec <- replicate(max(basis), 0)
    polyvec[basis] <- vector
    return(as.function(polynom::polynomial(polyvec)))
}

#' Evaluating polynomials
#'
#' This function allows one to evaluate a list of polynomials at
#' various points (the points are allowed to differ across
#' polynomials). This function will instead be used to evaluate a list
#' of monomials.
#' @param polynomials a list of polynomials.
#' @param points a list/vector of points at which we want to evaluate
#'     each polynomial.
#' @return A matrix of values, corresponding to the polynomials
#'     specified in \code{polynomials} evaluated at the points
#'     specified in \code{points}.
polylisteval <- function(polynomials, points) {
    if (is.list(polynomials)) {
        if (length(polynomials) != length(points)) {
            stop(gsub("\\s+", " ",
                      "List of polynomials to evaluate, and list of points to
                  evaluate each polynomial, are not equal."))
        }
        output <- mapply(predict, polynomials, points)
    } else {
        output <- predict(polynomials, points)
    }
    return(output)
}

#' Parsing marginal treatment response formulas
#'
#' This function takes in an MTR formula, and then parses the formula
#' such that it becomes a polynomial in the unobservable \code{u}. It
#' then breaks these polynomials into monomials, and then integrates
#' each of them with respect to \code{u}. Each integral corresponds to
#' E[md | D, X, Z].
#' @param formula the MTR.
#' @param data \code{data.frame} for which we obtain E[md | D, X, Z]
#'     for each observation.
#' @param uname variable name for unobservable used in declaring the
#'     MTR.
#' @param as.function boolean, if \code{FALSE} then a list of the
#'     polynomial terms are returned; if \code{TRUE} then a list of
#'     functions corresponding to the polynomials are returned.
#' @return A list (of lists) of monomials corresponding to the
#'     original MTR (for each observation); a list (of lists) of the
#'     integrated monomials; a vector for the degree of each of the
#'     original monomials in the MTR; and a vector for the names of
#'     each variable entering into the MTR (note \code{x^2 + x} has
#'     only one term, \code{x}).
polyparse.mst <- function(formula, data, uname = u, as.function = FALSE) {

    ## update formula parsing
    formula <- Formula::as.Formula(formula)
    uname   <- deparse(substitute(uname))

    ## Include redundant variable u, so monomials in m0, m1
    ## specifications correspond polynomial coefficients on u
    ## monomials
    data[[uname]] <- 1
    dmat <- design.mst(formula, data)$X

    ## Save original terms for reference as later
    oterms <- attr(terms(formula), "term.labels")

    ## Separate and parse the original terms
    nterms <- oterms
    for (sep in c("I\\(", "\\)", "\\*", ":", "  ", "  ")) {
        nterms <- lapply(nterms, gsub, pattern = sep, replacement = " ")
    }
    nterms <- strsplit(trimws(unlist(nterms)), " ")

    ## Find monomials of degree 1 ('degree' is with respect to u)
    u_pos <- lapply(nterms, whichforlist, obj = uname)
    u_pos <- which(u_pos > 0)

    ## Find monomials of degree exceeding 1, and determine their degree
    trunc_nterms <- lapply(nterms, substr, 0, nchar(uname) + 1)
    uexp_pos     <- lapply(trunc_nterms, whichforlist, obj = paste0(uname, "^"))
    uexp_pos     <- which(uexp_pos > 0)
    uexp_subpos  <- lapply(trunc_nterms[uexp_pos],
                           whichforlist, obj =  paste0(uname, "^"))
    deggtr2      <- FALSE
    if (length(unlist(uexp_subpos)) > 0) deggtr2 <- TRUE

    ## Create a matrix stating the degree for each monomial with degree >= 1
    if(length(u_pos) == 0){
        exptab1 <- NULL
    } else {
        exptab1 <- cbind(u_pos, 1)
    }
    if (deggtr2) {
        uexp <- as.numeric(mapply(vecextract,
                                  nterms[uexp_pos],
                                  position = uexp_subpos,
                                  truncation = nchar(uname) + 1))
        exptab2 <- cbind(uexp_pos, uexp)
        exptab  <- rbind(exptab1, exptab2)
    } else {
        exptab <- exptab1
    }

    if(!is.null(exptab)) {
        colnames(exptab) <- c("term", "degree")
    }

    ## Determine which terms do not involve u
    nonuterms    <- unlist(oterms[!seq(1, length(oterms)) %in% exptab[, 1]])
    nonutermspos <- which(oterms %in% nonuterms)

    if (length(nonuterms) > 0) {
        exptab0 <- cbind(nonutermspos, replicate(length(nonutermspos), 0))
    } else {
        exptab0  <- NULL
    }
    exptab <- rbind(exptab0, exptab)
    exptab <- exptab[order(exptab[, 1]), ]

    if (is.matrix(exptab)) {
        exporder <- exptab[, 2]
        colnames(exptab) <- c("term", "degree")
    } else {
        exporder <- exptab[2]
        names(exptab) <- c("term", "degree")
    }
    names(exporder) <- NULL

    ## generate matrix with monomial coefficients
    if ("(Intercept)" %in% colnames(dmat)) {
        exporder <- c(0, exporder)
        oterms   <- c("(Intercept)", oterms)
    }
    polymat <- as.matrix(dmat[, oterms])

    ## prepare monomials and their integrals
    polynomial_list <- lapply(split(polymat, seq(1, nrow(polymat))),
                              genpoly,
                              basis = exporder)

    if (as.function == FALSE) {
        monomial_list <- lapply(split(polymat, seq(1, nrow(polymat))),
                                genmono,
                                basis = exporder)

        integral_list <- lapply(monomial_list,
                                lapply,
                                polynom::integral)
        names(integral_list) <- rownames(data)
    } else {
        monomial_list <- lapply(split(polymat, seq(1, nrow(polymat))),
                                genmono,
                                basis = exporder,
                                as.function = TRUE)

        integral_list <- NULL
    }

    names(monomial_list) <- rownames(data)

    return(list(plist = polynomial_list,
                mlist = monomial_list,
                ilist = integral_list,
                exporder = exporder,
                terms = oterms))
}

#' Function to multiply polynomials
#'
#' This function takes in two vectors characterizing polynomials. It
#' then returns a vector characterizing the product of the two
#' polynomials.
#' @param poly1 vector, characerizing a polynomial.
#' @param poly2 vector, characerizing a polynomial.
#' @return vector, characterizing the product of the two polynomials
#'     characterized \code{poly1} and \code{poly2}.
polyProduct <- function(poly1, poly2) {
    poly1 <- c(1, 2, 3)
    poly2 <- c(2, 0, -1)

    degreeAdd <- seq(0, length(poly2) - 1)
    maxAdd <- max(degreeAdd)

    prodMat <- sapply(poly2, function(x) x * poly1)
    prodMat <- sapply(degreeAdd, function(x) c(rep(0, x),
                                               prodMat[, x + 1],
                                               rep(0, maxAdd - x)))
    return(rowSums(prodMat))
}

#' Estimating expectations of terms in the MTR (gamma objects)
#'
#' This function generates the gamma objects defined in the paper,
#' i.e. each additive term in E[md], where md is a MTR.
#'
#' @param monomials object containing list of list of monomials. Each
#'     element of the outer list represents an observation in the data
#'     set, each element in the inner list is a monomial from the
#'     MTR. The variable is the unobservable u, and the coefficient is
#'     the evaluation of any interactions with u.
#' @param lb vector of lower bounds for the interval of
#'     integration. Each element corresponds to an observation.
#' @param ub vector of upper bounds for the interval of
#'     integration. Each element corresponds to an observation.
#' @param multiplier a vector of the weights that enter into the
#'     interal. Each element corresponds to an observation.
#' @param subset Subset condition used to select observations with
#'     which to estimate gamma.
#' @param means logical, if TRUE then function returns the terms of
#'     E[md]. If FALSE, then function instead returns each term of
#'     E[md | D, X, Z]. This is useful for testing the code,
#'     i.e. obtaining population estimates.
#' @return If \code{means = TRUE}, then the function returns a vector
#'     of the additive terms in Gamma (i.e. the expectation is over D,
#'     X, Z, and u). If \code{means = FALSE}, then the function
#'     returns a matrix, where each row corresponds to an observation,
#'     and each column corresponds to an additive term in E[md | D, X,
#'     Z] (i.e. only the integral with respect to u is performed).
#'
#' @export
gengamma.mst <- function(monomials, lb, ub, multiplier = 1,
                         subset = NULL, means = TRUE) {

    exporder  <- monomials$exporder
    integrals <- monomials$ilist

    if (!is.null(subset)) integrals <- integrals[subset]

    nmono <- length(exporder)

    ## Determine bounds of integrals (i.e. include weights)
    if (length(ub) == 1) ub <- replicate(length(integrals), ub)
    if (length(lb) == 1) lb <- replicate(length(integrals), lb)

    ub <- split(replicate(nmono, ub), seq(length(ub)))
    lb <- split(replicate(nmono, lb), seq(length(lb)))
 
    monoeval <- t(mapply(polylisteval, integrals, ub)) -
        t(mapply(polylisteval, integrals, lb))

    termsN <- length(integrals[[1]])
    if (termsN == 1) monoeval <- t(monoeval)
 
    ## The object monoeval is supposed to have as many rows as the
    ## number of observations used for estimation. However, if the
    ## provided MTR objects include only one term, R transposes the
    ## matrix. So below I undo that transpose if the number of terms
    ## is 1. 

    preGamma <- sweep(monoeval,
                      MARGIN = 1,
                      STATS = multiplier,
                      FUN = "*")

    if (means) {
        if (is.matrix(preGamma)) {
            gstar <- colMeans(preGamma)
        } else {
            gstar <- mean(preGamma)
        }
        names(gstar) <- monomials$terms

        return(gstar)
    } else {
        return(preGamma)
    }
}

#' Separating splines from MTR formulas
#'
#' This function separates out the function call \code{uSplines()}
#' potentially embedded in the MTR formulas from the rest of the
#' formula. The terms involving splines are treated separately from
#' the terms that do not involve splines when creating the gamma
#' moments.
#' @param formula the formula that is to be parsed.
#' @return a list containing two objects. One object is \code{formula}
#'     but with the spline components removed. The second object is a
#'     list. The name of each element is the \code{uSplines()}
#'     command, and the elements are a vector of the names of
#'     covariates that were interacted with the \code{uSplines()} command.
#'
#' @examples
#' \dontrun{
#' Below is an example of how to declare a spline component.
#' m0 = ~ x1 + x1 : uSplines(degree = 2,
#'                           knots = c(0.2, 0.4)) +
#'             x2 : uSplines(degree = 2,
#'                           knots = c(0.2, 0.4)) +
#'             x1 : x2 : uSplines(degree = 2,
#'                                knots = c(0.2, 0.4)) +
#'             uSplines(degree = 3,
#'                      knots = c(0.2, 0.4),
#'                      intercept = FALSE)
#' }
removeSplines <- function(formula) {

    fterms <- attr(terms(formula), "term.labels")
    finter <- attr(terms(formula), "intercept")

    whichspline <- sapply(fterms,
                          function(y) grepl(x = y, pattern = "uSplines\\("))

    if (max(whichspline) == 1) {
        ftobj <- terms(formula)
        splinespos <- which(whichspline == TRUE)

        if (length(splinespos) == length(fterms)) {
            if (finter == 0) nosplines <- NULL
            if (finter == 1) nosplines <- ~ 1
        } else {
            nosplines <- drop.terms(ftobj, splinespos)
            nosplines <- Formula::as.Formula(nosplines)
        }

        splineterms <- fterms[whichspline]
        splineslist <- list()

        for (splineobj in splineterms) {

            splinespos <- regexpr("uSplines\\(", splineobj)
            degreepos  <- regexpr("degree = ", splineobj)
            knotspos   <- regexpr("knots = ", splineobj)
            knotslpos  <- regexpr("knots = c\\(", splineobj)
            interpos   <- regexpr("intercept = ", splineobj)

            substr(splineobj, splinespos, nchar(splineobj))

            ## Check if vectors or sequences are declared to adjust parsing
            firstopen  <- regexpr("\\(",
                                  substr(splineobj,
                                         splinespos + 8,
                                         nchar(splineobj)))
            firstclose <- regexpr("\\)",
                                  substr(splineobj,
                                         splinespos,
                                         nchar(splineobj)))
            secondclose <- regexpr("\\)",
                                   substr(splineobj,
                                          splinespos + 8 + firstclose,
                                          nchar(splineobj)))

            if ((firstopen < firstclose) & (secondclose != - 1)) {
                splinecmd <- substr(splineobj,
                                    splinespos,
                                    splinespos + 8 + firstclose + secondclose)
            } else {
                splinecmd <- substr(splineobj,
                                    splinespos,
                                    splinespos + firstclose - 1)
            }

            ## Separate uSplines command from terms interacting with the spline
            splinecmdstr <- gsub("\\)", "\\\\)",
                                 gsub("\\(", "\\\\(", splinecmd))

            interobj <- gsub(paste0(":", splinecmdstr), "", splineobj)
            interobj <- gsub(paste0(splinecmdstr, ":"), "", interobj)
            interobj <- gsub("::", ":", interobj)

            if (interobj == splinecmd) interobj <- "1"

            if (splinecmd %in% names(splineslist)) {
                splineslist[[splinecmd]] <- c(splineslist[[splinecmd]], interobj)
            } else {
                splineslist[[splinecmd]] <- c(interobj)
            }
        }

    } else {
        nosplines <- formula
        splineslist <- NULL
    }

    return(list(formula = nosplines,
                splineslist = splineslist))
}

#' Integrated splines
#'
#' This function integrates out splines that the user specifies when
#' declaring the MTRs. This is to be used when generating the gamma
#' moments.
#' @param x the points to evaluate the integral of the the splines.
#' @param knots the knots of the spline.
#' @param degree the degree of the spline; default is set to 0
#'     (constant splines).
#' @param intercept boolean, set to TRUE if intercept term is to be
#'     included (i.e. an additional basis such that the sum of the
#'     splines at every point in \code{x} is equal to 1).
#' @return a matrix, the values of the integrated splines. Each row
#'     corresponds to a value of \code{x}; each column corresponds to
#'     a basis defined by the degrees and knots.
#'
#' @examples
#' \dontrun{
#' Since the splines are declared as part of the MTR, you will need
#' to have parsed out the spline command. Thus, this command will be
#' called via eval(parse(text = .)). In the examples below, the
#' commands are parsed from the object \code{splineslist} generated
#' by \code{\link[MST]{removeSplines}}. The names of the elements in
#' the list are the spline commands, and the elements themselves are
#' the terms that interact with the splines.
#'
#' eval(parse(text = gsub("uSplines\\(",
#'                        "uSplinesInt(x = x, ",
#'                        names(splineslist)[1])))
#'
#' eval(parse(text = gsub("uSplines\\(",
#'                        "uSplinesInt(x = x, ",
#'                         names(splineslist)[2])))
#' }
uSplinesInt <- function(x, knots, degree = 0, intercept = TRUE) {
    
    splines2::ibs(x = x,
                  knots = knots,
                  degree = degree,
                  intercept = intercept,
                  Boundary.knots = c(0, 1))
}

#' Spline basis function
#'
#' This function evaluates the splines that the user specifies when
#' declaring the MTRs. This is to be used for auditing, namely when
#' checking the boundedness and monotonicity conditions.
#' @param x the points to evaluate the integral of the the splines.
#' @param knots the knots of the spline.
#' @param degree the degree of the spline; default is set to 0
#'     (constant splines).
#' @param intercept boolean, set to TRUE if intercept term is to be
#'     included (i.e. an additional basis such that the sum of the
#'     splines at every point in \code{x} is equal to 1).
#' @return a matrix, the values of the integrated splines. Each row
#'     corresponds to a value of \code{x}; each column corresponds to
#'     a basis defined by the degrees and knots.
#'
#' @examples
#' \dontrun{
#' Since the splines are declared as part of the MTR, you will need
#' to have parsed out the spline command. Thus, this command will be
#' called via eval(parse(text = .)). In the examples below, the
#' commands are parsed from the object \code{splineslist} generated
#' by \code{\link[MST]{removeSplines}}. The names of the elements in
#' the list are the spline commands, and the elements themselves are
#' the terms that interact with the splines.
#'
#' eval(parse(text = gsub("uSplines\\(",
#'                        "uSplinesBasis(x = x, ",
#'                         names(splineslist)[1])))
#'
#' eval(parse(text = gsub("uSplines\\(",
#'                        "uSplinesBasis(x = x, ",
#'                        names(splineslist)[2])))
#' }
uSplinesBasis <- function(x, knots, degree = 0, intercept = TRUE) {
    splines2::bSpline(x = x,
                      knots = knots,
                      degree = degree,
                      intercept = intercept,
                      Boundary.knots = c(0, 1))
}


#' Generate Gamma moments for splines
#'
#' The user can declare that the unobservable enters into the MTRs in
#' the form of splines. This function generates the gamma moments for
#' the splines. The specifications for the spline must be passed as an
#' element generated by \code{\link{removeSplines}}. This function
#' accounts for the interaction between covariates and splines.
#' @param splines a list generated by \code{\link{removeSplines}}
#'     applied to either the \code{m0} and \code{m1} argument.
#' @param data a \code{data.frame} object containing all the variables
#'     that interact with the spline components.
#' @param lb vector of lower bounds for the interval of
#'     integration. Each element corresponds to an observation.
#' @param ub vector of upper bounds for the interval of
#'     integration. Each element corresponds to an observation.
#' @param multiplier a vector of the weights that enter into the
#'     integral. Each element corresponds to an observation.
#' @param subset Subset condition used to select observations with
#'     which to estimate gamma.
#' @param d either 0 or 1, indicating the treatment status.
#' @return a matrix, corresponding to the splines being integrated
#'     over the region specified by \code{lb} and \code{ub},
#'     accounting for the interaction terms. The number of rows is
#'     equal to the number of rows in \code{data}. The number of
#'     columns depends on the specifications of the spline. The name
#'     of each column takes the following form: "u[d]S[j].[b]", where
#'     "u" and "S" are fixed and stand for "unobservable" and
#'     "Splines" respectively. "[d]" will be either 0 or 1, depending
#'     on the treatment status. "[j]" will be an integer indicating
#'     which element of the list \code{splines} the column pertains
#'     to. "[b]" will be an integer reflect which component of the
#'     basis the column pertains to.
genGammaSplines.mst <- function(splines, data, lb, ub, multiplier = 1,
                                subset, d = NULL, means = TRUE) {

    splines <- splines$splineslist

    if (is.null(splines)) {
        return(NULL)
    } else {
        if (!hasArg(subset)) {
            subset <- replicate(nrow(data), TRUE)
        }

        if (length(lb) == 1) lb <- replicate(nrow(data[subset, ]), lb)
        if (length(ub) == 1) ub <- replicate(nrow(data[subset, ]), ub)

        splinesGamma <- NULL
        splinesNames <- NULL

        for (j in 1:length(splines)) {

            ## Design matrix for covariates
            if ("1" %in% splines[[j]]) {
                nonSplineFormula <- as.formula(paste("~",
                                                     paste(splines[[j]],
                                                           collapse = " + ")))
            } else {
                nonSplineFormula <- as.formula(paste("~ 0 + ",
                                                     paste(splines[[j]],
                                                           collapse = " + ")))
            }
            nonSplinesDmat <- design.mst(nonSplineFormula,
                                         data[subset, ])$X

            ## Spline integral matrices
            splinesLB <- eval(parse(text = gsub("uSplines\\(",
                                                "uSplinesInt(x = lb, ",
                                                names(splines)[j])))
            
            splinesUB <- eval(parse(text = gsub("uSplines\\(",
                                                "uSplinesInt(x = ub, ",
                                                names(splines)[j])))
            splinesInt <- splinesUB - splinesLB           

            ## Combine the design and integral matrices
            for (l in 1:length(splines[[j]])) {

                tmpGamma <- sweep(splinesInt,
                                  MARGIN = 1,
                                  STATS = nonSplinesDmat[, l],
                                  FUN = "*")

                tmpGamma <- sweep(x = tmpGamma,
                                  MARGIN = 1,
                                  STATS = multiplier,
                                  FUN = "*")

                splinesNames <- c(splinesNames,
                                  paste0(paste0("u", d, "S", j, "."),
                                         seq(1, ncol(tmpGamma)),
                                         paste0(":", splines[[j]][l])))
                splinesGamma <- cbind(splinesGamma, tmpGamma)
            }
        }

        if (means == TRUE) {
            splinesGamma <- colMeans(splinesGamma)
            names(splinesGamma) <- splinesNames
        } else {
            colnames(splinesGamma) <- splinesNames
        }
        return(splinesGamma)
    }
}

#' Generate basis matrix for splines
#'
#' The user can declare that the unobservable enters into the MTRs in
#' the form of splines. This function generates the basis matrix for
#' the splines. The specifications for the spline must be passed as
#' the \code{$splineslist} object generated by
#' \code{\link{removeSplines}}. Note that this function does not
#' account for any interactions between the splines and the
#' covariates. Interactions can be added simply by sweeping the basis
#' matrix by a vector for the values of the covariates.
#' @param splines a list. The name of each element should be the
#'     spline command, and each element should be a vector. Each entry
#'     of the vector is a covariate that the spline should be
#'     interacted with. Such an object can be generated by
#'     \code{\link{removeSplines}}, and accessed using
#'     \code{$splineslist}.
#' @param x the values of the unobservable at which the splines basis
#'     should be evaluated.
#' @param d either 0 or 1, indicating the treatment status.
#' @return a matrix. The number of rows is equal to the length of
#'     \code{x}, and the number of columns depends on the
#'     specifications of the spline. The name of each column takes the
#'     following form: "u[d]S[j].[b]", where "u" and "S" are fixed and
#'     stand for "unobservable" and "Splines" respectively. "[d]" will
#'     be either 0 or 1, depending on the treatment status. "[j]" will
#'     be an integer indicating which element of the list
#'     \code{splines} the column pertains to. "[b]" will be an integer
#'     reflect which component of the basis the column pertains to.
genBasisSplines.mst <- function(splines, x, d = NULL) {

    if (is.null(splines)) {
        return(NULL)
    } else {

        bmatList <- list()

        for (j in 1:length(splines)) {
            splinesBasis <- NULL
            splinesNames <- NULL

            bmat <- eval(parse(text = gsub("uSplines\\(",
                                           "uSplinesBasis(x = x, ",
                                           names(splines[j]))))

            colnames(bmat) <- paste0(paste0("u", d, "S", j, "."),
                                     seq(1, ncol(bmat)))

            bmatList[[j]] <- bmat
        }
        return(bmatList)
    }
}

#' Evaluate a particular function
#'
#' This function evaluates a single function in a list of functions.
#' @param fun the function to be integrated.
#' @param values the values of the arguments to the function.
#' @param argnames the argument names corresponding to \code{values}.
#' @return the output of the function evaluated.
funEval <- function(fun, values = NULL, argnames = NULL) {
    if (!is.null(values) & !is.null(argnames)) {
        args <- as.list(values)
        names(args) <- argnames
        do.call(fun, args)
    } else {
        do.call(fun, list())
    }
}

#' Construct constant function
#'
#' This function constructs another function that returns a
#' constant. It is used for constructing weight/knot functions.
#' @param x scalar, the constant the function evaluates to.
#' @return a function.
constructConstant <- function(x) {
    fun <- function(...) {
        x
    }
    return(fun)
}
