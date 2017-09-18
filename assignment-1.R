# ===========================
# Mandatory Excercise 1
# Written by Christian Stigen
# ===========================

# Clear any existing environment
rm(list=ls())

# A variadic helper-function to print stuff to the console. Meaning you can run
# with Rscript.
println <- function(...) {
  cat(paste(..., "\n", sep=""))
}

println("Problem 1 (b)")
println("P(X > 4000) = ", 1 - pexp(4000, rate=1/5000))
println("P(4000 <= X <= 6000) = ", pexp(6000, rate=1/5000)
        - pexp(4000, rate=1/5000))

# Simulate that the lifetime is more than 4000 hours
println("Problem 1 (c)")

# Returns the probability (via simulation) that a lightbulb lives longer than
# 4000 hours.
P_longer <- function(x, sims, beta=5000) {
  sum(rexp(sims, 1/beta) > x) / sims
}

println("P(X > 4000) 10 simulations: ", P_longer(4000, 10))
println("P(X > 4000) 100 simulations: ", P_longer(4000, 100))
println("P(X > 4000) 10000 simulations: ", P_longer(4000, 10000))
println("P(X > 4000) 100000 simulations: ", P_longer(4000, 100000))

# Comment: We definitely see that the larger number of samples, the closer we
# get to the theoretical correct answer. However, it also becomes more costly
# to computate, at least the way we do it here. The winnings for doing it this
# way will only be seen when the underlying function is not known (i.e. not
# exactly) or computationally very expensive.

P_sum <- function(sims, beta=5000) {
  sum((rexp(sims, 1/beta) + rexp(sims, 1/beta)) > 10000) / sims
}

println("Problem 1 (d)")
println("P(X + X > 10000) 10 simulations: ", P_sum(10))
println("P(X + X > 10000) 100 simulations: ", P_sum(100))
println("P(X + X > 10000) 1000 simulations: ", P_sum(1000))
println("P(X + X > 10000) 10000 simulations: ", P_sum(10000))
println("P(X + X > 10000) 100000 simulations: ", P_sum(100000))
println("P(X + X > 10000) 1000000 simulations: ", P_sum(1000000))
println("P(X + X > 10000) 10000000 simulations: ", P_sum(10000000))

println("Problem 1 (e)")
println("Exact: ", 1 - pgamma(10000, shape=2, rate=1/5000))
