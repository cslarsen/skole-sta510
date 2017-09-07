rm(list=ls())

f <- function(x, k=3/4) {
  if ( -1 <= x && x <= 1 ) {
    k*(1-x^2)
  } else {
    0
  }
}

x <- seq(-2, 2, 0.01)
y <- mapply(f, x)
plot(x, y, ylab="f(x)", type="l")
