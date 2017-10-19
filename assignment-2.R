# ==============================
# STA-510: Mandatory Excercise 2
# UiS, October 2017
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

problem2b <- function(runs=100) {
  # Samples from a homogeneous Poisson process (HPP). The multiplier is to make
  # sure we get samples in the entire range [a, b].
  rhpp <- function(lambda, a, b, multiplier=3) {
    n <- multiplier * (b-a) * lambda
    w <- a + cumsum(rexp(n, lambda))
    w[w <= b]
  }

  # Samples from a non-homogeneous Poisson process (NHPP) in the range [a, b].
  # The lambda.fun parameter is the big lambda function (expected value of
  # intensity) and inverse.fun is the reverse of that function.
  rnhpp <- function(a, b, lambda.fun, inverse.fun) {
    # First sample from HPP. Remember to transform [a,b] to values in the HPP
    # space.
    w <- rhpp(lambda=1, a=lambda.fun(a), b=lambda.fun(b))

    # Return the arrival values transformed back to NHPP space
    s <- inverse.fun(w)
  }

  # The pre-calculated big lambda and its inverse
  lambda.fun <- function(t) { 10*t^(7/5) }
  inverse.fun <- function(w) { 10^(-5/7) * w^(5/7) }

  # Get arrival times (x-values in the plot)
  s <- rnhpp(0, 5, lambda.fun, inverse.fun)

  plot(s, 1:length(s), type="s", lwd=1.5,
       xlab="Arrival time",
       ylab="Event number",
       main="Non-homogeneous Poisson process (NHPP)")

  # Show the points as well
  points(s, rep(0, length(s)), pch=16, cex=0.5, col="red")
}

problem2b_cinlars_method <- function() {
  rm(list=ls())

  inverse <- function(w) {
    (10^(-5/7)) * (w^(5/7))
  }
  Nsim <- 100
  s <- 0
  out <- c()
  for ( i in 1:Nsim) {
    u <- runif(1)
    s <- s - log(u)
    t <- inverse(s)
    out <- c(out, t)
  }

  out <- out[out<=5]
  plot(out, 1:length(out), type="s",
       main="Cinlar's method for NHPP",
       xlab="Arrival times",
       ylab="Event number")
  points(out, rep(0, length(out)), pch=16, cex=0.5, col="red")
}

problem2b_thinning <- function() {
  # NHPP via thinning.
  # NOTE: Code taken from lecturer.

  simtNHPP <- function(a,b,lambdamax,lambdafunc){
    if(max(lambdafunc(seq(a,b,length.out = 100)))>lambdamax)
      stop("lambdamax is smaller than max of the lambdafunction")

    expectednumber <- (b-a)*lambdamax
    Nsim <- 3*expectednumber
    timesbetween <- rexp(Nsim,lambdamax)
    timesto <- a+cumsum(timesbetween)
    timesto <- timesto[timesto<b]
    Nevents <- length(timesto)

    # Thinning
    U <- runif(Nevents)
    timesto <- timesto[U<lambdafunc(timesto)/lambdamax]
    timesto
  }

  intensity <- function(t) {
    14*t^0.4
  }

  # Generate data with the traffic intensity and plot them
  NHPPtimes <- simtNHPP(a=0, b=5, lambdamax=100, lambdafunc=intensity)
  NHPPtimes <- NHPPtimes[NHPPtimes <= 5]

  plot(NHPPtimes, 1:length(NHPPtimes), type="s",
       xlab = "Arrival time",
       ylab = "Event number",
       lwd=1.5)

  points(NHPPtimes, rep(0, length(NHPPtimes)), pch=16, cex=0.5, col="red")
}

problem2d <- function() {
  rhpp <- function(lambda, a, b, multiplier=3) {
    n <- multiplier * (b-a) * lambda
    w <- a + cumsum(rexp(n, lambda))
    w[w <= b]
  }

  # See above
  rnhpp <- function(a, b, lambda.fun, inverse.fun) {
    inverse.fun(rhpp(lambda=1, a=lambda.fun(a), b=lambda.fun(b)))
  }

  # The pre-calculated big lambda and its inverse
  lambda.fun <- function(t) { 10*t^(7/5) }
  inverse.fun <- function(w) { 10^(-5/7) * w^(5/7) }

  # Calc mean number of failures during the first year
  runs <- 10000
  failures <- 0
  for ( i in 1:runs ) {
    s <- rnhpp(0, 1, lambda.fun, inverse.fun)
    failures <- failures + length(s[s<1])
  }
  println("Mean failures during first year: ", (sum(failures) / runs),
          " (", runs, " runs)")

  # Probability of more than 10 pump failures during first year
  runs <- 10000
  hits <- 0
  for ( i in 1:runs ) {
    s <- rnhpp(0, 1, lambda.fun, inverse.fun)
    hits <- hits + (length(s) > 10)
  }
  println("P(failures > 10 first year) = ", (hits/runs))
  println()

  # Calc mean number of failures during the last year
  failures <- 0
  runs <- 10000
  for ( i in 1:runs ) {
    s <- rnhpp(4, 5, lambda.fun, inverse.fun)
    failures <- failures + length(s[s>=4 && s<5])
  }
  println("Mean failures during last year: ", (sum(failures) / runs))
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
  n <- 10000
  e <- 100 # error less than 100 from actual theta
  alpha = (1 - 0.95) # confidence level
  X <- runif(n, min=a, max=b)
  sigma.hat <- sum((g(X) - mean(g(X)))^2) / (n - 1)
  n.hat <- ceiling(sigma.hat * (qnorm(alpha/2)*(b - a) / e )^2)
  println("Approximation of sigma.hat")
  println()
  println("  sigma.hat      = ", sigma.hat, " (", n, " runs)")
  println("  ceiling(n.hat) = ", n.hat, " (alpha=", alpha, " e=", e, ")")
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
    abs(theta - theta.hat) <= e
  }

  N <- 1000
  good.diffs <- sum(mapply(calc_diff, 1:N))
  empirical.alpha <- good.diffs / N
  println("Over ", N, " runs, how many times did our theta.hat fall within")
  println("e = ", e, " of the actual theta ?")
  println()
  println("  empirical.alpha = ", empirical.alpha)
  println()
}

g <- function(t) {
  25 + 50*t*sin(t*2*pi/24)^2 + 250*sin(t*pi/24)^2
}

problem3c_plot <- function(a=0, b=24) {
  x <- seq(a, b, length.out=100)
  plot(x, g(x), type="l")
}

problem3c <- function() {
  # Bounds
  a <- 0
  b <- 24

  # Find maximum
  t.max <- optimize(g, interval=c(a, b), maximum=TRUE)
  c <- ceiling(g(t.max$maximum))
  println("Maximum value of g(x) between [", a, ", ", b, "]")
  println("  c = ", c, " (ceiling at t = ", t.max$maximum, ")")
  println()

  # Estimate p.hat
  n <- 10000 # chosen arbitrarily; we could probably calculate that as well
  Y <- runif(n, min=0, max=c)
  X <- runif(n, min=a, max=b)
  p.hat <- mean(Y <= g(X))

  println("Estimation of p")
  println("  p.hat = ", p.hat, " (over ", n, " runs)")
  println()

  # Estimate the number of simulations required for HM Monte Carlo
  e <- 100
  alpha = (1 - 0.95)
  n.hat <- ceiling((p.hat*(1 - p.hat)) * (qnorm(alpha/2) * c*(b - a) / e)^2)
  println("Estimation of n for HM Monte Carlo")
  println("  e = ", e)
  println("  alpha = ", alpha)
  println("  ceiling(n.hat) = ", n.hat, " (number of runs for HM)")
}

problem3d <- function() {
  # Bounds
  a <- 0
  b <- 24
  c <- 1052

  # Confidence interval (just for printing here; rest derived from problem3c)
  e <- 100
  alpha <- (1 - 0.95)

  # The number of simulations, taken from problem3c (rounded up a small amount)
  n <- 60000
  X <- runif(n, min=a, max=b)
  Y <- runif(n, min=0, max=c)
  theta.hat.hm <- c*(b - a) * mean(Y <= g(X))

  println("Estimation of theta.hat.hm with ", 100*(1-alpha),
          "% conf. int. for e = ", e)
  println("  theta.hat.hm = ", theta.hat.hm, " (", n, " runs)")

  # As before, measure the empirical alpha
  integral <- function(t) {
    (25*(pi*(pi*t*(t + 12) - 120 * sin(pi*t/12) - 12 * t * sin(pi*t/6)) - 72 *
         cos(pi*t/6))) / (2*pi^2)
  }

  theta.exact = integral(b) - integral(a)
  println("  theta.exact  = ", theta.exact)
  println("  difference   = ", abs(theta.exact - theta.hat.hm))
  println()

  # Now try to see how many times the difference is within e, to double-check
  # our estimate of n
  calc_diff <- function(dummy) {
    n <- 60000
    X <- runif(n, min=a, max=b)
    Y <- runif(n, min=0, max=c)
    theta.hat.hm <- c*(b - a) * mean(Y <= g(X))
    abs(theta.exact - theta.hat.hm) <= e
  }

  N <- 1000
  good.diffs <- sum(mapply(calc_diff, 1:N))
  empirical.alpha <- good.diffs / N
  println("Over ", N, " runs, how many times did our theta.hat.hm fall within")
  println("e = ", e, " of the actual theta ?")
  println()
  println("  empirical.alpha = ", empirical.alpha)
  println()
}
