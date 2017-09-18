# Mandatory Excercise 1
# Written by Christian Stigen

# Clear any existing environment
rm(list=ls())

# A variadic helper-function to print stuff to the console. Meaning you can run
# with Rscript.
println <- function(...) {
  cat(paste(..., "\n", sep=""))
}

# Problem 1 (b)
println("Problem 1 (b) P(X > 4000) = ", 1 - pexp(4000, rate=1/5000))
