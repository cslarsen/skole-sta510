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

problem1b <- function() {
  # covariance matrix
  sigma <- matrix(c( 900, -240,  24,
                    -240,  100, -12,
                      24,  -12,  16), ncol=3)
  mean <- c(90, 48, 18)

  x <- rmvnorm(n=100000, sigma=sigma, mean=mean)
  cat(colMeans(x), "\n")
  cat(var(x), "\n")
  plot(x)

  # X1 ~ N(my=90, var=900)
  # X2 ~  48, 100
  # X3 ~ 18, 16
  # P (X1 > 100, X2 > 50, X3 > 20)
}
