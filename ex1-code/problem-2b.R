par(mfrow=c(1,1))
rm(list=ls())

f <- function(x) {
  ((x+1)^2) / 28
}

barplot(f(0:3), names=0:3, ylab="f", xlab="x")
