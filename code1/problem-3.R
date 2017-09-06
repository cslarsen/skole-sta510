iterations <- 1e4

# Simulated expected value for a PMF
sev <- function(domain, weights, samples=iterations) {
  total <- 0

  for (i in 1:samples) {
    draw <- sample(domain, size=1, prob=weights)
    total <- total + draw
  }

  average <- total / samples
}

# Exact expected value for a PMF
eev <- function(domain, weights) {
  sum(domain * weights)
}

cat("Results for", iterations, "iterations per simulation\n\n")

f <- function(x) { x/10 }
g <- function(x) { x^3 }
D <- 1:4 # domain

cat("(2a) E(X) simulated    ", sev(D, f(D)), "\n")
cat("(2a) E(X) exact        ", eev(D, f(D)), "\n\n")
cat("(2a) E(g(X)) simulated ", sev(g(D), f(D)), "\n")
cat("(2a) E(g(X)) exact     ", eev(g(D), f(D)), "\n\n")

f <- function(x, c=1/30) { c*(x+1)^2 }
D <- 0:3 # domain

cat("(2b) E(X) simulated    ", sev(D, f(D)), "\n")
cat("(2b) E(X) exact        ", eev(D, f(D)), "\n\n")
cat("(2b) E(g(X)) simulated ", sev(g(D), f(D)), "\n")
cat("(2b) E(g(X)) exact     ", eev(g(D), f(D)), "\n")
