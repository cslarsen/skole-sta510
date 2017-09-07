rm(list=ls())

f <- function(x) {
  if ( 0 <= x && x <= 1) {
    4*x*(1-x^2)
  } else {
    0
  }
}

F <- function(x) {
  if ( x < 0 ) {
    0
  } else if ( x > 1 ) {
    1
  } else {
    2*x^2 - x^4
  }
}

par(mfrow=c(2,1))
x <- seq(-0.5, 1.5, 0.01)
plot(x, mapply(f, x), type="l", ylab="f")
title("Probability density function")
plot(x, mapply(F, x), type="l", ylab="F")
title("Cumulative density function")
