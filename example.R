rm(list = ls())
set.seed(10L)
devtools::load_all(pkg = "IVMTE", reset = TRUE)

##------------------------
## Run MST estimator
##------------------------

## Just so you can see the variable names and what you're dealing with.
?dtsf ## Hm, documentation needs work.
head(dtsf)

## This just indexes a random subsample of the population data that I
## generated. I threw this in just to speed up run time while debugging.
bsample <- sample(x = seq(1, nrow(dtsf)),
                  size = 200)

## So this is how you declare your S-set of IV-like estimands: a
## vector of R-style formulas.
ivlike <- c(ey ~ d,
            ey ~ d + x,
            ey ~ d + x | z + x)

## So I use this "lists.mst" function to declare the components from
## the IV-like estimands I want to use as references. The reason is
## because I wanted to follow the R convention where variable names do
## not need to be quoted when used as arguments to a function (e.g. in
## formulas). The problem is that, things outside of quotes are
## treated as variables, and R will try to find them. This will either
## lead to something else being submitted as the argument, or an error
## telling me that the variable doesn't exist. So lists.mst() was my
## way to get around that problem. Feel free to suggest other
## alternatives, or let us know if you have a way we can do without
## lists.mst()!
components <- lists.mst(c(intercept, d), d, c(d, x))

## A couple things here:
##
## 1. The 'propensity' argument is for characterizing the
## probability of taking up treatment. Here, I declare that variable
## 'p' in the data already has those propensity scores
## calculated. Alternatively, you can submit a formula, like:
## d ~ x + z
## This formula characterizes the model for the propensity score you
## have in mind. You can set the argument "link" to "probit", "logit",
## or "linear". If you have a more complicated model for the propensity,
## thats when you should first estimate those propensity scores,
## store them in a variable, and then pass the variable name into the
## function.
##
## 2. Splines for the unobservable component can be declared using
## these "uSplines" function-like things (they're not actually
## functions, just a cheeky way to get things parsed). All possible
## arguments are shown below. Note how you can interact the entire
## spline with a covariate, and you can include multiple splines.
##
## 3. Okay, I realize explaining all the audit-related
## (i.e. obseq.tol, grid.Nu, grid.Nx, etc.) arguments this way is too
## confusing. It's probably much easier to explain in person, and will
## take less time. Let me know if Magne/Alex hasn't covered that
## change in the paper with you yet.

result <- ivmte(ivlike = ivlike,
                data = dtsf[bsample, ],
                components = components,
                propensity = p,
                m1 = ~ x + uSplines(degree = 2,
                                    knots = c(0.3, 0.6),
                                    intercept = FALSE),
                m0 = ~ 0 + x : uSplines(degree = 0,
                                        knots = c(0.2, 0.5, 0.8),
                                        intercept = TRUE) +
                    uSplines(degree = 1,
                             knots = c(0.4),
                             intercept = TRUE) +
                    I(u ^ 2),
                uname = u,
                target = "att",
                obseq.tol = 1.01,
                grid.Nu = 3,
                grid.Nx = 2,
                audit.Nx = 1,
                audit.Nu = 5,
                m1.ub = 55,
                m0.lb = 0,
                mte.inc = TRUE)


