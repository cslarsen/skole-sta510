# ==============================
# STA-510: Mandatory Excercise 1
# UiS, September 2017
# Written by Christian Stigen
# ==============================

# Clear any existing environment
rm(list=ls())

# A variadic helper-function to print stuff to the console. Meaning you can run
# with Rscript.
println <- function(...) {
  cat(..., "\n", sep="")
}

problem1b <- function() {
  println("P(X > 4000) = ", 1 - pexp(4000, rate=1/5000))
  println("P(4000 <= X <= 6000) = ", pexp(6000, rate=1/5000)
          - pexp(4000, rate=1/5000))
}

problem1c <- function() {
  # Simulate that the lifetime is more than 4000 hours

  # Returns the probability (via simulation) that a lightbulb lives longer than
  # 4000 hours.
  P_longer <- function(x, sims, beta=5000) {
    sum(rexp(sims, 1/beta) > x) / sims
  }

  println("P(X > 4000) 10 simulations: ", P_longer(4000, 10))
  println("P(X > 4000) 100 simulations: ", P_longer(4000, 100))
  println("P(X > 4000) 10000 simulations: ", P_longer(4000, 10000))
  println("P(X > 4000) 100000 simulations: ", P_longer(4000, 100000))
}

problem1d <- function() {
  P_sum <- function(sims, beta=5000) {
    sum((rexp(sims, 1/beta) + rexp(sims, 1/beta)) > 10000) / sims
  }

  println("P(X + X > 10000) 10 simulations: ", P_sum(10))
  println("P(X + X > 10000) 100 simulations: ", P_sum(100))
  println("P(X + X > 10000) 1000 simulations: ", P_sum(1000))
  println("P(X + X > 10000) 10000 simulations: ", P_sum(10000))
  println("P(X + X > 10000) 100000 simulations: ", P_sum(100000))
}

problem1e <- function() {
  println(1 - pgamma(10000, shape=2, rate=1/5000))
}

problem2b <- function() {
  lottosim <- function(runs) {
    hits <- 0

    for ( n in 1:runs ) {
      numbers <- sample(1:34, 7)
      consec <- min(diff(sort(numbers))) == 1
      hits <- hits + consec
    }

    hits / runs
  }

  println("n=10: ", lottosim(10))
  println("n=100: ", lottosim(1000))
  println("n=1000: ", lottosim(1000))
  println("n=10000: ", lottosim(10000))
  println("n=6614: ", lottosim(6614))
}

# Samples from a Rayleigh distribution.
rayleigh <- function(samples, theta) {
  # The inverse function g^{-1}(u)
  g <- function(u) {
    theta*sqrt(2*log(1/(1-u)))
  }

  # Generate `samples` random numbers. Runif doesn't include extreme values, so
  # will never get 0 or 1. We don't want 1, but it would be nice to be able to
  # get 0. I initially tried sample(1:10^decimals, samples) / 10^decimals,
  # where decimals typically was 6. That gives me numbers in the range
  # 0--0.999999, and seems to work very well in simulations. However, it is a
  # very slow way to generate floating point numbers, so I switched to runif.
  # Runif is much faster, but does not generate 0. However, because of the
  # large number of simulations, I get almost similar results to what I had
  # before.
  #
  # The runif source code is here:
  # https://github.com/wch/r-source/blob/e5b21d0397c607883ff25cca379687b86933d730/src/nmath/runif.c
  # Seems they use the regular rand-trick to get numbers in that range.
  g(runif(samples))
}

problem3b <- function(num_samples=1000000, theta=1.78) {
  samples <- rayleigh(num_samples, theta=theta)

  # Make a histogram
  hist(samples, main=paste(length(samples), "Rayleigh samples"), freq=FALSE,
       right=FALSE, xlab="Wave Heights", breaks=50)

  # Show expected value as a vertical line
  expected <- theta * sqrt(pi/2)
  variance <- theta^2 * (4 - pi) / 2
  abline(v=expected, col="red", lty=2, lwd=2)

  # Also plot the computer distribution
  rayleigh_pdf <- function(x) {
    (x/theta^2) * exp(-x^2/(2*theta^2))
  }
  curve(rayleigh_pdf, from=0, to=7, col="red", add=TRUE, lwd=2)

  # Compare with theoretically expected values

  n <- length(samples)
  sample_mean <- sum(samples) / n
  sample_var <- sum((samples - sample_mean)^2) / (n - 1)

  # Print a LaTeX table

  println("\\begin{table}")
  println("  \\centering")
  println("  \\begin{tabular}{@{}lcc@{}}")
  println("    \\toprule")
  println("     & Expected & Sampled \\\\")
  println("    \\midrule")
  println("    Mean", " & ", expected, " & ", sample_mean, " \\\\")
  println("    Variance", " & ", variance, " & ", sample_var, " \\\\")
  println("    \\bottomrule")
  println("  \\end{tabular}")
  println("  \\caption{Theoretical and sampled metrics for problem 3 (b). ")
  println("     ``Expected Mean'' is actually the \\textit{expected value}.}")
  println("  \\label{table:3b}")
  println("\\end{table}")
}

problem3d <- function() {
  simulate <- function(runs) {
    results <- vector(mode="logical", runs)
    for ( n in 1:runs ) {
      results[n] <- max(rayleigh(200, 1.3 )) > 5
    }
    sum(results) / runs
  }

  simv2 <- function(runs) {
    sum(rayleigh(200*runs, 1.3) > 5) / runs
  }

  println("100 simulations, wave > 5 prob: ", simulate(100))
  println("1000 simulations, wave > 5 prob: ", simulate(1000))
  println("10000 simulations, wave > 5 prob: ", simulate(10000))
  println("100000 simulations, wave > 5 prob: ", simulate(100000))
  println("100000 simulations v2, wave > 5 prob: ", simv2(100000))
  #println("1000000 simulations, wave > 5 prob: ", simulate(1000000))
}

ptriangle <- function(xs, a, b, c) {
  delta_left = 2 / ((b - a) * (c - a))
  delta_right = -2 / ((b - a) * (b - c))
  offset_left = -2*a / ((b - a) * (c - a))
  offset_right = 2*b / ((b - a) * (b - c))

  sapply(xs, function(x) {
    if ( x <= c ) {
      delta_left*x + offset_left
    } else {
      delta_right*x + offset_right
  }})
}

rtriangle <- function(n, a=1.5, b=3, c=2) {
  f <- function(x) {
    ptriangle(x, a, b, c)
  }

  M <- 2 / (b - a)
  samples <- numeric(n)

  for ( i in 1:n ) {
    repeat {
      u <- runif(1)
      y <- runif(1, min=a, max=b)
      if ( u < f(y)/M ) {
        break
      }
    }
    samples[i] <- y
  }

  samples
}

problem3f <- function() {
  samples <- rtriangle(100000)

  hist(samples, freq=FALSE, right=FALSE, breaks=100,
       main="Triangle Distribution",
       xlab="Wave Height")

  # Draw PDF
  x <- seq(1.5, 3, 0.01)
  f <- function(x) { ptriangle(x, a=1.5, b=3, c=2) }
  lines(x, f(x), col="red", add=TRUE, lwd=2)

  # Draw expected value
  abline(v=2, col="red", lty=2, lwd=2)
}
