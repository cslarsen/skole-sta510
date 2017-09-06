iterations <- 1e5

# Find the expected value of a PMF by returning the average of repeated,
# weighted samples.
sim_ev <- function(domain, pmf, samples=iterations) {
  weights = pmf(domain)
  total <- 0

  # Draws a weighted sample and updates the running total
  for (i in 1:samples) {
    drawn <- sample(domain, size=1, prob=weights)
    total <- total + drawn
  }

  average <- total / samples
  normalized <- average * sum(weights)
}

# Calculates E(X) for a PMF exactly by x*f(x) for all x in domain
exact_ev <- function(domain, pmf) {
  sum(domain * pmf(domain))
}

# Provide a domain and PMF to find E(X):

f <- function(x, c=10) { x/c }
g <- function(x) { x^3 }
domain <- 1:4

cat("Results for", iterations, "iterations per simulation\n\n")

cat("(2a) E(X) simulated    ", sim_ev(domain, f), "\n")
cat("(2a) E(X) exact        ", exact_ev(domain, f), "\n\n")

cat("(2a) E(g(X)) simulated ", sim_ev(g(domain), f), "\n")
cat("(2a) E(g(X)) exact     ", exact_ev(g(domain), f), "\n\n")

f <- function(x, c=1/30) { c*(x+1)^2 }
domain <- 0:3

cat("(2b) E(X) simulated    ", sim_ev(domain, f), "\n")
cat("(2b) E(X) exact        ", exact_ev(domain, f), "\n\n")

cat("(2b) E(g(X)) simulated ", sim_ev(g(domain), f), "\n")
cat("(2b) E(g(X)) exact     ", exact_ev(g(domain), f), "\n")
