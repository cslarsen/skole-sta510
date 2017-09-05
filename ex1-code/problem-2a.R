par(mfrow=c(1,1))
rm(list=ls())

f <- function(x) {
  x/10
}

barplot(f(1:4), ylab="f", xlab="x")
