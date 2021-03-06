#' Constructing LP problem
#'
#' This function takes in the IV estimates from the set of IV
#' regressions declared by the user, as well as their corresponding
#' moments of the terms in the MTR. These are then used to construct
#' the components that make up the LP problem. Additional constraint
#' matrix is added using \code{mbA} (\code{mb} stands for
#' "monotonicity/boundedness"); extra model sense is added using
#' \code{mbs}; extra RHS values added using \code{mbrhs}). Depending
#' on the linear programming solver used, this function will return
#' different output specific to the solver.
#' @param sset List of IV-like estimates and the corresponding gamma
#'     terms.
#' @param mbA Matrix used to define the constraints in the LP problem.
#' @param mbs Vector of model sense/inequalities signs used to define
#'     the constraints in the LP problem.
#' @param mbrhs Vector of constants used to define the constraints in
#'     the LP problem.
#' @param lpsolver string, name of the package used to solve the LP
#'     problem.
#' @return A list of matrices and vectors necessary to define an LP
#'     problem for Gurobi.
#'
#' @export
lpsetup.mst <- function(sset, mbA = NULL, mbs = NULL, mbrhs = NULL, lpsolver) {

    ## determine lengths
    sn  <- length(sset)
    gn0 <- length(sset$s1$g0)
    gn1 <- length(sset$s1$g1)

    ## generate all vectors/matrices for LP optimization to minimize
    ## observational equivalence
    obj <- c(replicate(sn * 2, 1),
             replicate(gn0 + gn1, 0))
    rhs <- unlist(lapply(sset, function(x) x[["beta"]]))
    sense <- replicate(sn, "=")

    A <- NULL
    scount <- 0
    for (s in names(sset)) {
        avec <- replicate(2 * sn, 0)
        avec[(2 * scount + 1) :(2 * scount + 2)] <- c(-1, 1)
        ## Regarding c(-1, 1), the -1 is for w+, 1 is for w-
        avec <- c(avec, sset[[s]]$g0, sset[[s]]$g1)
        A <- rbind(A, avec)
        scount <- scount + 1
    }

    colnames(A) <- c(seq(1, 2 * sn),
                     colnames(A)[(2 * sn + 1) : ncol(A)])

    ## Add in additional constraints if included
    A     <- rbind(A, mbA)
    sense <- c(sense, mbs)
    rhs   <- c(rhs, mbrhs)

    if (lpsolver == "Rcplex") {
        sense[sense == "<"]  <- "L"
        sense[sense == "<="] <- "L"
        sense[sense == ">"]  <- "G"
        sense[sense == ">="] <- "G"
        sense[sense == "="]  <- "E"
    }

    ## define bounds
    ub <- replicate(ncol(A), Inf)
    lb <- c(replicate(sn * 2, 0), replicate(gn0 + gn1, -Inf))

    ## If using lpSolve, convert the object into one of x+/x-
    if (lpsolver == "lpSolve") {
        obj <- c(obj, -obj[(sn * 2 + 1) : ncol(A)])
        A <- cbind(A, -A[, (sn * 2 + 1) : ncol(A)])
    }

    return(list(obj = obj,
                rhs = rhs,
                sense = sense,
                A = A,
                ub = ub,
                lb = lb,
                sn = sn,
                gn0 = gn0,
                gn1 = gn1))
}

#' Minimizing violation of observational equivalence
#'
#' Given a set of IV-like estimates and the set of matrices/vectors
#' defining an LP problem, this function minimizes the violation of
#' observational equivalence under the L1 norm.
#' @param sset A list of IV-like estimates and the corresponding gamma
#'     terms.
#' @param lpobj A list of matrices and vectors defining an LP problem.
#' @param lpsolver string, name of the package used to solve the LP
#'     problem.
#' @return A list including the minimum violation of observational
#'     equivalence, the solution to the LP problem, and the status of
#'     the solution.
#'
#' @export
obseqmin.mst <- function(sset, lpobj, lpsolver) {

    if (lpsolver == "gurobi") {
        model <- list()
        model$modelsense <- "min"
        model$obj   <- lpobj$obj
        model$A     <- lpobj$A
        model$rhs   <- lpobj$rhs
        model$sense <- lpobj$sense
        model$ub    <- lpobj$ub
        model$lb    <- lpobj$lb

        result   <- gurobi::gurobi(model, list(outputflag = 0))
        obseqmin <- result$objval
        optx     <- result$x
        status   <- result$status

    }
    if (lpsolver == "Rcplex") {
        result <- Rcplex::Rcplex(objsense = "min",
                                 cvec = lpobj$obj,
                                 Amat = lpobj$A,
                                 bvec = lpobj$rhs,
                                 sense = lpobj$sense,
                                 ub = lpobj$ub,
                                 lb = lpobj$lb,
                                 control = list(trace = FALSE))

        obseqmin <- result$obj
        optx     <- result$xopt
        status   <- result$status

    }

    if (lpsolver == "lpSolve") {
        result <- lpSolve::lp(direction = "min",
                              objective.in = lpobj$obj,
                              const.mat = lpobj$A,
                              const.rhs = lpobj$rhs,
                              const.dir = lpobj$sense)

        obseqmin <- result$objval
        optxA <- result$solution[1 : (lpobj$sn * 2)]
        optxB <- result$solution[(lpobj$sn * 2 + 1) : length(result$solution)]
        optxB <- optxB[1 : (length(optxB) / 2)] +
            optxB[(length(optxB) / 2 + 1) : length(optxB)]
        optx <- c(optxA, optxB)

        if (result$status == 0) status <- 1
        if (result$status != 0) status <- 0

    }

    if (lpsolver == "cplexAPI") {

        result <- runCplexAPI(lpobj, cplexAPI::CPX_MIN)

        obseqmin <- result$objval
        optx     <- result$optx
        status   <- result$status

    }

    if (lpsolver == "lpSolveAPI") {

        result <- runLpSolveAPI(lpobj, 'min')

        obseqmin <- result$objval
        optx     <- result$optx
        status   <- result$status

    }

    ## provide nicer output
    g0sol <- optx[(2 * lpobj$sn + 1) : (2 * lpobj$sn + lpobj$gn0)]
    g1sol <- optx[(2 * lpobj$sn + lpobj$gn0 + 1) :
                  (2 * lpobj$sn + lpobj$gn0 + lpobj$gn1)]

    names(g0sol) <- names(sset$gstar$g0)
    names(g1sol) <- names(sset$gstar$g1)

    ## return output
    return(list(obj = obseqmin,
                g0 = g0sol,
                g1 = g1sol,
                status = status,
                result = result))
}

#' Obtaining TE bounds
#'
#' This function estimates the bounds on the target treatment effect.
#' @param g0 set of expectations for each terms of the MTR for the
#'     control group.
#' @param g1 set of expectations for each terms of the MTR for the
#'     control group.
#' @param sset a list containing the point estimates and gamma
#'     components associated with each element in the S-set.
#' @param lpobj A list of matrices and vectors defining an LP problem.
#' @param obseq.tol tolerance level for how much more the solution is
#'     permitted to violate observational equivalence of the IV-like
#'     estimands.
#' @param noisy boolean, set to \code{TRUE} if optimization results
#'     should be displayed.
#' @param lpsolver string, name of the package used to solve the LP
#'     problem.
#' @return a list containing the bounds on the treatment effect; the
#'     coefficients on each term in the MTR associated with the upper
#'     and lower bounds, for both counterfactuals; the optimization
#'     status to the maximization and minimization problems; the LP
#'     problem that the optimizer solved.
#'
#' @export
bound.mst <- function(g0, g1, sset, lpobj, obseq.tol, lpsolver, noisy = FALSE) {

    if (lpsolver %in% c("gurobi", "Rcplex", "cplexAPI", "lpSolveAPI")) {

        ## define model
        model <- list()
        model$obj <- c(replicate(2 * lpobj$sn, 0), g0, g1)
        model$rhs <- c(obseq.tol, lpobj$rhs)
        model$ub    <- lpobj$ub
        model$lb    <- lpobj$lb

        avec <- c(replicate(2 * lpobj$sn, 1),
                  replicate(lpobj$gn0 + lpobj$gn1, 0))
        model$A <- rbind(avec, lpobj$A)
        model$sense <- c("<=", lpobj$sense)

        ## obtain lower and upper bounds
        if (lpsolver == "gurobi") {

            model$modelsense <- "min"
            minresult <- gurobi::gurobi(model, list(outputflag = 0))
            min <- minresult$objval
            minstatus <- 0
            if (minresult$status == "OPTIMAL") minstatus <- 1
            minoptx <- minresult$x

            model$modelsense <- "max"
            maxresult <- gurobi::gurobi(model, list(outputflag = 0))
            max <- maxresult$objval
            maxstatus <- 0
            if (maxresult$status == "OPTIMAL") maxstatus <- 1
            maxoptx <- maxresult$x
        }
        if (lpsolver == "Rcplex") {
            model$sense[1] <- "L"

            minresult <- Rcplex::Rcplex(objsense = "min",
                                        cvec = model$obj,
                                        Amat = model$A,
                                        bvec = model$rhs,
                                        sense = model$sense,
                                        ub = model$ub,
                                        lb = model$lb,
                                        control = list(trace = FALSE))

            min <- minresult$obj
            minstatus <- minresult$status
            minoptx   <- minresult$xopt

            maxresult <- Rcplex::Rcplex(objsense = "max",
                                        cvec = model$obj,
                                        Amat = model$A,
                                        bvec = model$rhs,
                                        sense = model$sense,
                                        ub = model$ub,
                                        lb = model$lb,
                                        control = list(trace = FALSE))

            max <- maxresult$obj
            maxstatus <- maxresult$status
            maxoptx   <- maxresult$xopt
        }

        if (lpsolver == "cplexAPI") {

            minresult <- runCplexAPI(model, cplexAPI::CPX_MIN)

            min       <- minresult$objval
            minoptx   <- minresult$optx
            minstatus <- minresult$status

            maxresult <- runCplexAPI(model, cplexAPI::CPX_MAX)

            max       <- maxresult$objval
            maxoptx   <- maxresult$optx
            maxstatus <- maxresult$status
        }

        if (lpsolver == "lpSolveAPI") {
            minresult <- runLpSolveAPI(model, 'min')

            min       <- minresult$objval
            minoptx   <- minresult$optx
            minstatus <- minresult$status

            maxresult <- runLpSolveAPI(model, 'max')

            max       <- maxresult$objval
            maxoptx   <- maxresult$optx
            maxstatus <- maxresult$status
        }
    }

    if (lpsolver == "lpSolve") {

        ## define model
        model <- list()
        model$obj <- c(replicate(2 * lpobj$sn, 0), g0, g1, -g0, -g1)
        model$rhs <- c(obseq.tol, lpobj$rhs)
        model$sense <- c("<=", lpobj$sense)

        avec <- c(replicate(2 * lpobj$sn, 1),
                  replicate(2 * (lpobj$gn0 + lpobj$gn1), 0))

        model$A <- rbind(avec, lpobj$A)

        ## obtain upper and lower bounds
        minresult <- lpSolve::lp(direction = "min",
                                 objective.in = model$obj,
                                 const.mat = model$A,
                                 const.rhs = model$rhs,
                                 const.dir = model$sense)

        min <- minresult$objval
        optxA <- minresult$solution[1 : (lpobj$sn * 2)]
        optxB <- minresult$solution[(lpobj$sn * 2 + 1) :
                                    length(minresult$solution)]
        optxB <- optxB[1 : (length(optxB) / 2)] +
            optxB[(length(optxB) / 2 + 1) : length(optxB)]
        minoptx <- c(optxA, optxB)

        if (minresult$status == 0) minstatus <- 1
        if (minresult$status != 0) minstatus <- 0

        maxresult <- lpSolve::lp(direction = "max",
                                 objective.in = model$obj,
                                 const.mat = model$A,
                                 const.rhs = model$rhs,
                                 const.dir = model$sense)

        max <- maxresult$objval
        optxA <- maxresult$solution[1 : (lpobj$sn * 2)]
        optxB <- maxresult$solution[(lpobj$sn * 2 + 1) :
                                    length(maxresult$solution)]
        optxB <- optxB[1 : (length(optxB) / 2)] +
            optxB[(length(optxB) / 2 + 1) : length(optxB)]
        maxoptx <- c(optxA, optxB)

        if (maxresult$status == 0) maxstatus <- 1
        if (maxresult$status != 0) maxstatus <- 0
    }

    ming0 <- minoptx[(2 * lpobj$sn + 1) : (2 * lpobj$sn + lpobj$gn0)]
    ming1 <- minoptx[(2 * lpobj$sn + lpobj$gn0 + 1) :
                     (2 * lpobj$sn + lpobj$gn0 + lpobj$gn1)]

    maxg0 <- maxoptx[(2 * lpobj$sn + 1) : (2 * lpobj$sn + lpobj$gn0)]
    maxg1 <- maxoptx[(2 * lpobj$sn + lpobj$gn0 + 1) :
                     (2 * lpobj$sn + lpobj$gn0 + lpobj$gn1)]

    names(ming0) <- names(sset$gstar$g0)
    names(ming1) <- names(sset$gstar$g1)

    names(maxg0) <- names(sset$gstar$g0)
    names(maxg1) <- names(sset$gstar$g1)

    if (noisy) {
        cat("Min status:", minstatus, "\n")
        cat("Max status:", maxstatus, "\n")
        cat("Bound: (", min, ",", max, ")\n")
    }

    return(list(max = max,
                maxg0 = maxg0,
                maxg1 = maxg1,
                maxresult = maxresult,
                maxstatus = maxstatus,
                min = min,
                ming0 = ming0,
                ming1 = ming1,
                minresult = minresult,
                minstatus = minstatus,
                model = model))
}

#' Running cplexAPI LP solver
#'
#' This function solves the LP problem using the cplexAPI package. The
#' object generated by \code{\link{lpsetup.mst}} is not compatible
#' with the \code{cplexAPI} functions. This function adapts the object
#' to solve the LP problem.
#' @param lpobj list of matrices and vectors defining the linear
#'     programming problem.
#' @param lpdir input either CPX_MAX or CPX_MIN, which sets the LP
#'     problem as a maximization or minimization problem.
#' @return a list of the output from CPLEX. This includes the
#'     optimization status, the objective value, the solution vector,
#'     amongst other things.
runCplexAPI <- function(lpobj, lpdir) {
    env  <- cplexAPI::openEnvCPLEX()
    prob <- cplexAPI::initProbCPLEX(env)
    cplexAPI::chgProbNameCPLEX(env, prob, "sample")

    sense <- lpobj$sense
    sense[sense == "<"]  <- "L"
    sense[sense == "<="] <- "L"
    sense[sense == ">"]  <- "G"
    sense[sense == ">="] <- "G"
    sense[sense == "="]  <- "E"
    sense[sense == "=="] <- "E"

    ub <- lpobj$ub
    ub[ub == Inf] <- cplexAPI::CPX_INFBOUND

    lb <- lpobj$lb
    lb[lb == -Inf] <- - cplexAPI::CPX_INFBOUND

    cnt <- apply(lpobj$A, MARGIN = 2, function(x) length(which(x != 0)))

    beg <- rep(0, ncol(lpobj$A))
    beg[-1] <- cumsum(cnt[-length(cnt)])

    ind <- unlist(apply(lpobj$A, MARGIN = 2, function(x) which(x != 0) - 1))

    val <- c(lpobj$A)
    val <- val[val != 0]

    cplexAPI::copyLpwNamesCPLEX(env = env,
                                lp = prob,
                                nCols = ncol(lpobj$A),
                                nRows = nrow(lpobj$A),
                                lpdir = lpdir,
                                objf = lpobj$obj,
                                rhs = lpobj$rhs,
                                sense = sense,
                                matbeg = beg,
                                matcnt = cnt,
                                matind = ind,
                                matval = val,
                                lb = lb,
                                ub = ub)

    cplexAPI::lpoptCPLEX(env, prob)
    solution <- cplexAPI::solutionCPLEX(env, prob)
    cplexAPI::delProbCPLEX(env, prob)
    cplexAPI::closeEnvCPLEX(env)

    if (typeof(solution) == "S4") {
        if (attr(solution, "class") == "cplexError") {
            status <- 0
            solution <- list()
            solution$objval <- NA
            solution$x <- NA
        }
    }  else {
        if (solution$lpstat == 1) status <- 1
        if (solution$lpstat != 1) status <- 0
    }

    return(list(objval = solution$objval,
                optx   = solution$x,
                status = status))
}

#' Running lpSolveAPI
#'
#' This function solves the LP problem using the \code{lpSolveAPI}
#' package. The object generated by \code{\link{lpsetup.mst}} is not
#' compatible with the \code{lpSolveAPI} functions. This function
#' adapts the object to solve the LP problem.
#' @param lpobj list of matrices and vectors defining the linear
#'     programming problem.
#' @param modelsense input either 'max' or 'min' which sets the LP
#'     problem as a maximization or minimization problem.
#' @return a list of the output from \code{lpSolveAPI}. This includes
#'     the optimization status, the objective value, the solution
#'     vector.
runLpSolveAPI <- function(lpobj, modelsense) {
    lpmodel <- lpSolveAPI::make.lp(nrow(lpobj$A), ncol(lpobj$A))

    for (j in 1:ncol(lpobj$A)) {
        lpSolveAPI::set.column(lprec = lpmodel,
                               column = j,
                               x = lpobj$A[, j])
    }

    lpSolveAPI::set.constr.value(lprec = lpmodel,
                                 rhs = lpobj$rhs)

    sense <- lpobj$sense
    sense[sense == "<"]  <- "<="
    sense[sense == ">"]  <- ">="
    sense[sense == "=="] <- "="

    lpSolveAPI::set.constr.type(lprec = lpmodel,
                                types = sense)

    lpSolveAPI::set.objfn(lprec = lpmodel,
                          obj = lpobj$obj)

    lpSolveAPI::lp.control(lprec = lpmodel,
                           sense = modelsense)

    lpSolveAPI::set.bounds(lprec = lpmodel,
                           lower = lpobj$lb,
                           upper = lpobj$ub)

    solved <- lpSolveAPI::solve.lpExtPtr(lpmodel)
    if (solved == 0) status <- 1
    if (solved != 0) status <- 0

    return(list(objval = lpSolveAPI::get.objective(lpmodel),
                optx   = lpSolveAPI::get.variables(lpmodel),
                status = status))
}
