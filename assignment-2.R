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
  # Covariance matrix, as calculated in problem 1 (a).
  sigma <- matrix(c( 900, -240,  24,
                    -240,  100, -12,
                      24,  -12,  16), ncol=3)
  # Mu-vector
  mean <- c(90, 48, 18)

  # Perform simulation
  x <- rmvnorm(n=runs, sigma=sigma, mean=mean)
  x1 <- x[,1]
  x2 <- x[,2]
  x3 <- x[,3]

  # P (X1 > 100, X2 > 50, X3 > 20)
  println("P(X1 > 100, X2 > 50, X3 > 20) = ",
          (sum(x1 > 100 & x2 > 50 & x3 > 20) / runs),
          " (", runs, " simulations)")

  # X1 ~ N(my=90, var=900)
  # X2 ~  48, 100
  # X3 ~ 18, 16
}
