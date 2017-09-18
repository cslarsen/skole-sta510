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
println("P(4000 <= X <= 6000) = ",
        pexp(6000, rate=1/5000) - pexp(4000, rate=1/5000))
