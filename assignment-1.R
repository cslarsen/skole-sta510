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
