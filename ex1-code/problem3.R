# Finds expected value of a probability mass function through sampling.
# Written by Christian Stigen for the course STA-510, autumn 2017

# Given a function f(x) and its domain of x-values, finds the expected value
# through simulation.
#
# It simply draws weighted samples and return the average value.
sim_expected <- function(f, domain, samples=10000) {
  weights = f(domain)
  total <- 0

  for (i in 1:samples) {
    drawn <- sample(domain, 1, prob=weights)
    total <- total + drawn
  }

  average <- total / samples
}

# Try with functions from problem 2

E <- sim_expected(function(x, c=10) x/c, 0:3)
cat("Expected value (2a):", E, "\n")

E <- sim_expected(function(x, c=1/30) c*(x+1)^2, 1:4)
cat("Expected value (2b):", E, "\n")
