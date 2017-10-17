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

problem1b_alternative <- function() {
  # Covariance matrix from problem 1 (a).
  sigma <- matrix(c( 900, -240,  24,
                    -240,  100, -12,
                      24,  -12,  16), ncol=3)
  # Expectation vector from problem 1 (a).
  mean <- c(90, 48, 18)

  println("Alternative P(X1 > 100, X2 > 50, X3 > 20) = ",
          pmvnorm(mean=mean, sigma=sigma,
                  lower=c(100,  50,  20),
                  upper=c(Inf, Inf, Inf)))

  println("Alternative P(X1 < 100, X2 > 50, X3 < 15) = ",
          pmvnorm(mean=mean, sigma=sigma,
                  lower=c(-Inf,  50, -Inf),
                  upper=c( 100, Inf,   15)))
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

# Samples from a non-homogeneous Poisson distribution
rnhpp <- function(runs, inverse) {
  cumsum(inverse(runif(runs)))
}

problem2b <- function(runs=100) {
  # Start and stop times
  a <- 0
  b <- 5

  inverse <- function(w) {
    (-log(1 - w))^(5/7)
  }

  x <- rnhpp(runs, inverse)
  plot(seq(min(x), max(x), length.out=length(x)), cumsum(x), type="s",
       xlab="Time in years", ylab="Cumulative failures")
  summary(x)
}

problem3b <- function() {
  # Integrand
  g <- function(t) {
    25 + 50*t*sin(t*2*pi/24)^2 + 250*sin(t*pi/24)^2
  }

  # Bounds
  a <- 0
  b <- 24

  # Approximate standard deviation
  n <- 1500
  e <- 100 # error less than 100 from actual theta
  alpha = (1 - 0.95) # confidence level
  X <- runif(n, min=a, max=b)
  sigma.hat <- sum((g(X) - mean(g(X)))^2) / (n - 1)
  n.hat <- ceiling(sigma.hat * (qnorm(alpha/2)*(b - a) / e )^2)
  println("Approximation of sigma.hat")
  println()
  println("  sigma.hat      = ", sigma.hat, " (", n, " runs)")
  println("  ceiling(n.hat) = ", n.hat, " (alpha=0.05 e=100)")
  println()

  # Approximate integral
  x <- runif(n.hat, min=a, max=b)
  theta.hat <- (b - a) * mean(g(x))

  # The exact integral (from Wolfram Alpha)
  integral <- function(t) {
    (25*(pi*(pi*t*(t + 12) - 120 * sin(pi*t/12) - 12 * t * sin(pi*t/6)) - 72 *
         cos(pi*t/6))) / (2*pi^2)
  }

  println("Approximation and exact value of integral")
  theta = integral(b) - integral(a)
  println()
  println("  theta.hat  = ", theta.hat, " (", n.hat, " runs)")
  println("  theta      = ", theta, " (from Wolfram Alpha)")
  println("  difference = ", abs(theta - theta.hat))
  println()

  # Now try to see how many times the difference is within e, to double-check
  # our estimate of n
  calc_diff <- function(dummy) {
    theta.hat <- (b - a) * mean(g(runif(n.hat, min=a, max=b)))
    abs(theta - theta.hat) < e
  }

  N <- 250
  good.diffs <- sum(mapply(function(dummy) { calc_diff(n.hat) }, 1:N))
  empirical.alpha <- good.diffs / N
  println("Over ", N, " runs, how many times did our theta.hat fall within")
  println("e = ", e, " of the actual theta ?")
  println()
  println("  empirical.alpha = ", empirical.alpha)
  println()

  if ( empirical.alpha < (1 - alpha) ) {
    println("Suggestion: Set n > ", n, " ?")
  }
}
