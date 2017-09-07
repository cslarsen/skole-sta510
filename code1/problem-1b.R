rm(list=ls())

simheads <- function(Nsim, k) {
  nheads <- vector(length=Nsim)

  for(i in 1:Nsim)
    nheads[i] <- sum(sample(0:1, size=k, replace=TRUE))

  return (nheads)
}

par(mfrow=c(3,1))

for (Nsim in c(10, 20, 1000)) {
  k <- 5
  nheadsim <- simheads(Nsim=Nsim, k=k)
  relfreq <- table(nheadsim )/ Nsim

  barplot(relfreq,
          ylab="Relative frequency",
          xlab=paste("Heads in ", k, " fair coin flips (", Nsim, " simulations)",
                     sep=""))
}
