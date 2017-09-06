sim_expected <- function(domain, f, samples=1e4) {
  weights = f(domain)
  total <- 0

  # Draws a weighted sample and updates the running total
  for (i in 1:samples) {
    drawn <- sample(domain, 1, prob=weights)
    total <- total + drawn
  }

  average <- total / samples
}

# Provide a domain and PMF to find E(X):

E <- sim_expected(1:4, function(x, c=10) { x/c })
cat("(2a) Expected value:", E, "\n")

E <- sim_expected(0:3, function(x, c=1/30) { c*(x+1)^2 })
cat("(2a) Expected value:", E, "\n")
