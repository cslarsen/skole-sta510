# ==============================
# STA-510: Mandatory Excercise 1
# UiS, September 2017
# Written by Christian Stigen
# ==============================
#
# Each problem has been made into a separate function. For example, you can
# call problem1b() to see the code for that.

# Clear any existing environment
rm(list=ls())

# Utility function for printing to the console
println <- function(...) {
  cat(..., "\n", sep="")
}

# Install required packages
if ( !"mtvnorm" %in% rownames(installed.packages()) ) {
  println("=========================================================")
  println("You do not have the mtvnorm package. Will install it now.")
  println("=========================================================")
  install.packages("mtvnorm")
}

problem1b <- function() {
}
