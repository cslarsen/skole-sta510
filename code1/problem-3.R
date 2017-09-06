iterations <- 1e5

# Approximates E(X) for a PMF by trial
sim_ev <- function(domain, weights, samples=iterations) {
  total <- 0

  # Draws a weighted sample and updates the running total
  for (i in 1:samples) {
    drawn <- sample(domain, size=1, prob=weights)
    total <- total + drawn
  }

  average <- total / samples
}

# Calculates exact E(X) for a PMF
exact_ev <- function(domain, weights) {
  sum(domain * weights)
}

cat("Results for", iterations, "iterations per simulation\n\n")

f <- function(x) { x/10 }
g <- function(x) { x^3 }
domain <- 1:4

cat("(2a) E(X) simulated    ", sim_ev(domain, f(domain)), "\n")
cat("(2a) E(X) exact        ", exact_ev(domain, f(domain)), "\n\n")
cat("(2a) E(g(X)) simulated ", sim_ev(g(domain), f(domain)), "\n")
cat("(2a) E(g(X)) exact     ", exact_ev(g(domain), f(domain)), "\n\n")

f <- function(x, c=1/30) { c*(x+1)^2 }
domain <- 0:3

cat("(2b) E(X) simulated    ", sim_ev(domain, f(domain)), "\n")
cat("(2b) E(X) exact        ", exact_ev(domain, f(domain)), "\n\n")
cat("(2b) E(g(X)) simulated ", sim_ev(g(domain), f(domain)), "\n")
cat("(2b) E(g(X)) exact     ", exact_ev(g(domain), f(domain)), "\n")
