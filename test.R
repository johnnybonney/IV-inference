#' File to create dataset and run tests

#directory <- "C:/Users/John Bonney/Desktop/Magne_projects/iv_inference/IV-inference"
setwd(directory)

rm(list = ls())
set.seed(10L)
devtools::load_all(pkg = "IVMTE", reset = TRUE)

library(data.table)
test_data <- data.table(x = rnorm(4200, mean = 0.5, sd = 0.1), #not sure what we really want here
                        z = sample(1:4, 4200, replace=T),
                        u = runif(4200, 0, 1))


test_data[, ey0 := 0.9 - 1.1 * u + 0.3 * u^2] #MTR from Mogstad and Torgovitsky (ARE) p. 582 -- is this how we want to do it? (Josh uses splines, but I can't find "uSpline" function)
test_data[, ey1 := 0.35 - 0.3 * u - 0.05 * u^2]

#assign probability of treatment uptake, p
iv_assignment <- data.table(z = 1:4, p = c(0.12, 0.29, 0.48, 0.78))
setkey(iv_assignment, z)
setkey(test_data, z)
test_data <- iv_assignment[test_data]

test_data[, d := as.integer(u <= p)] #equation 4
test_data[, ey := d * ey1 + (1 - d) * ey0] #equation 1