rm(list=ls())

f <- function(x) {
  if ( 0 <= x && x <= 1) {
    4*x*(1-x^2)
  } else {
    0
  }
}

x <- seq(-0.5, 1.5, 0.01)
plot(x, mapply(f, x), type="l", ylab="f")
