# Find the expected value of a PMF by returning the average of repeated,
# weighted samples.
sim_expected <- function(domain, pmf, samples=1e4) {
  weights = pmf(domain)
  total <- 0

  # Draws a weighted sample and updates the running total
  for (i in 1:samples) {
    drawn <- sample(domain, size=1, prob=weights)
    total <- total + drawn
  }

  average <- total / samples
}

# Provide a domain and PMF to find E(X):

f <- function(x, c=10) { x/c }
EX <- sim_expected(1:4, f)
Eg <- sim_expected(1:4, function(x) { f(x)^3 })

cat("(2a) E(X) =", EX, "\n")
cat("(2a) E(g(X)) =", Eg, "\n\n")

f <- function(x, c=1/30) { c*(x+1)^2 }
EX <- sim_expected(0:3, f)
Eg <- sim_expected(0:3, function(x) { f(x)^3 })

cat("(2b) E(X) =", EX, "\n")
cat("(2b) E(g(X)) =", Eg, "\n")
