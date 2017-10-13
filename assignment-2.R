# ==============================
# STA-510: Mandatory Excercise 1
# UiS, September 2017
# Written by Christian Stigen
# ==============================
#
# Each problem has been made into a separate function. For example, you can
# call problem1b() to see the code for that.

# Clear any existing environment
rm(list=ls())

# Utility function for printing to the console
println <- function(...) {
  cat(..., "\n", sep="")
}

# Install required packages
if ( !"mvtnorm" %in% rownames(installed.packages()) ) {
  println("==========================================================")
  println("You do not have the mvtnorm package. You should install it")
  println("==========================================================")
  quit(1)
} else {
  library(mvtnorm)
}

problem1b <- function(runs=1000000) {
  # Covariance matrix from problem 1 (a).
  sigma <- matrix(c( 900, -240,  24,
                    -240,  100, -12,
                      24,  -12,  16), ncol=3)
  # Expectation vector from problem 1 (a).
  mean <- c(90, 48, 18)

  # Perform simulation
  x <- rmvnorm(n=runs, sigma=sigma, mean=mean)
  x1 <- x[,1]
  x2 <- x[,2]
  x3 <- x[,3]

  # The first one is a top-tier character
  println("P(X1 > 100, X2 > 50, X3 > 20) = ",
          (sum(x1 > 100 & x2 > 50 & x3 > 20) / runs),
          " (", runs, " simulations)")

  println("P(X1 < 100, X2 > 50, X3 < 15) = ",
          (sum(x1 < 100 & x2 > 50 & x3 < 15) / runs),
          " (", runs, " simulations)")

  # NOTE: We could write sum((x %*% c(1, 2, 5)) > 300) instead, but it yields
  # the exact same result.
  println("P(X1 + 2X2 + 5X3 > 300)       = ",
          (sum((x1 + 2*x2 + 5*x3) > 300) / runs),
          " (", runs, " simulations)")
}

problem1d <- function(runs=1000000) {
  # Expectation vector from problem 1 (a).
  mean <- c(90, 48, 18)

  # Covariance matrices for the three difference scenarios

  sigma1 <- matrix(c( 900, -240,   0,
                     -240,  100,   0,
                        0,    0,  16), ncol=3)

  sigma2 <- matrix(c( 900,    0,   0,
                        0,  100,   0,
                        0,    0,  16), ncol=3)

  sigma3 <- matrix(c( 900,  240,   0,
                      240,  100,   0,
                        0,    0,  16), ncol=3)

  # Simulate the three scenarios

  scenario1 <- rmvnorm(n=runs, sigma=sigma1, mean=mean)
  scenario2 <- rmvnorm(n=runs, sigma=sigma2, mean=mean)
  scenario3 <- rmvnorm(n=runs, sigma=sigma3, mean=mean)

  display <- function(label, x) {
    x1 <- x[,1]
    x2 <- x[,2]
    x3 <- x[,3]

    println(label, " P(X1 > 100, X2 > 50, X3 > 20) = ",
            (sum(x1 > 100 & x2 > 50 & x3 > 20) / runs))
  }

  println("Results for ", runs, " simulations")
  display("(i)  ", scenario1)
  display("(ii) ", scenario2)
  display("(iii)", scenario3)
}
