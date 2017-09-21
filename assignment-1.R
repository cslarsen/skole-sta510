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
  P_longer <- function(x, runs, beta=5000) {
    sum(rexp(runs, 1/beta) > x) / runs
  }

  println("P(X > 4000) 10 simulations: ", P_longer(4000, 10))
  println("P(X > 4000) 100 simulations: ", P_longer(4000, 100))
  println("P(X > 4000) 10000 simulations: ", P_longer(4000, 10000))
  println("P(X > 4000) 100000 simulations: ", P_longer(4000, 100000))
}

problem1d <- function() {
  P_sum <- function(runs, beta=5000) {
    sum((rexp(runs, 1/beta) + rexp(runs, 1/beta)) > 10000) / runs
  }

  println("P(X + X > 10000) 10 simulations: ", P_sum(10))
  println("P(X + X > 10000) 100 simulations: ", P_sum(100))
  println("P(X + X > 10000) 1000 simulations: ", P_sum(1000))
  println("P(X + X > 10000) 10000 simulations: ", P_sum(10000))
  println("P(X + X > 10000) 100000 simulations: ", P_sum(100000))
  println("P(X + X > 10000) 1000000 simulations: ", P_sum(1000000))
}

problem1e <- function() {
  println("Exact P(X + X > 10000) = ", 1 - pgamma(10000, shape=2, rate=1/5000))
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
  println("95% certain of error at most 0.01 n=6614: ", lottosim(6614))
}

# Samples from a Rayleigh distribution.
rayleigh <- function(samples, theta) {
  # The inverse function g^{-1}(u)
  g <- function(u) {
    theta*sqrt(2*log(1/(1-u)))
  }

  # Generate `samples` random numbers, and pass them all through the inverse
  # function g. Note that runif doesn't include extreme values of 0 and 1, so
  # our simulation will be a little bit off. See the report for more details.
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
    count <- 0
    for ( n in 1:runs ) {
      count <- count + (max(rayleigh(200, 1.3)) > 5)
    }
    count / runs
  }

  println("100 simulations, wave > 5 prob: ", simulate(100))
  println("1000 simulations, wave > 5 prob: ", simulate(1000))
  println("10000 simulations, wave > 5 prob: ", simulate(10000))
  println("100000 simulations, wave > 5 prob: ", simulate(100000))

  # The following two takes a LONG time to execute, but show that we converge
  # to the correct value calculated in problem 3 (g).
  println("1000000 simulations, wave > 5 prob: ", simulate(1000000))
  #println("10000000 simulations, wave > 5 prob: ", simulate(10000000))
}

ptriangle <- function(x, a, b, c) {
  delta_left <- 2 / ((b - a) * (c - a))
  delta_right <- -2 / ((b - a) * (b - c))
  offset_left <- -2*a / ((b - a) * (c - a))
  offset_right <- 2*b / ((b - a) * (b - c))

  # We use sapply in case we get several values: Then we can use this function
  # for example with ptriangle(seq(1.5,3,0.1)) and so on.
  sapply(x, function(x) {
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

  its_until_acc <- 0
  samples <- numeric(n)

  C <- 2 / (b-a)

  for ( i in 1:n ) {
    repeat {
      y <- runif(1)
      x <- runif(1, min=a, max=b)
      its_until_acc <- its_until_acc + 1
      if ( y <= f(x)/C ) {
        break
      }
    }
    samples[i] <- x
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
  lines(x, f(x), col="red", lwd=2)

  # Draw expected value
  abline(v=2, col="red", lty=2, lwd=2)
}

problem3h <- function() {
  heights <- rtriangle(100000)

  maxheight <- function(y, theta, m) {
    1 - (1 - exp(-y^2/(2*theta^2)))^m
  }

  #println("Max height y=5, theta=1.3, m=200: ", #maxheight(5, 1.3, 200))

  prob_crit <- numeric(length(heights))

  for ( i in 1:length(heights) ) {
    expected_height <- heights[i]
    theta <- expected_height / sqrt(pi/2)
    maxh <- maxheight(7, theta, 800)
    prob_crit[i] <- maxh
  }

  println("Generated ", length(heights), " maximum height samples.")

  # We will make a recommendation based on a confidence interval
  m <- mean(prob_crit)
  s <- sd(prob_crit)

  println("")
  println("Mean probability of critical wave: ", m)
  println("Standard deviation of critical wave probability: ", s)

  hsum <- summary(prob_crit)
  println("")
  println("Minimum:      ", hsum[1])
  println("1st Quartile: ", hsum[2])
  println("Median:       ", hsum[3])
  println("Mean:         ", hsum[4])
  println("3rd Quartile: ", hsum[5])
  println("Maximum:      ", hsum[6])

  # 95% confidence interval
  error <- qt(0.0975,
              df=length(prob_crit)-1)*sd(prob_crit)/sqrt(length(prob_crit))
  interval <- c(m+error, m-error)
  println("")
  println("95% confidence interval [", interval[1], ", ", interval[2], "]")

  cutoff = 0.1
  println("")
  println("We require that the confidence interval extreme is below 0.1")
  if ( max(interval) < 0.1 ) {
    println("Recommendation: GO AHEAD")
  } else {
    println("Recommendation: DO NOT PROCEED")
  }
}
