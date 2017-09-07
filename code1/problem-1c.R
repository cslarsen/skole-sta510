# Lecture code example modified by Christian Stigen

# Reset environment for interactive runs
rm(list=ls())

# As in problem 1 (b), but with 0.6 chance of heads.
simheads <- function(Nsim,k) {
  nheads <- vector(length=Nsim)
  for(i in 1:Nsim)
    nheads[i] <- sum(sample(0:1, size=k, replace=TRUE, prob=c(0.4, 0.6)))
  return(nheads)
}

par(mfrow=c(3,1))

for (Nsim in c(10, 20, 1000)) {
  k <- 5
  nheadsim <- simheads(Nsim=Nsim,k=k)
  relfreq <- table(nheadsim) / Nsim
  barplot(relfreq,
          ylab="Relative frequency",
          xlab=paste("Biased heads in ", k, " coin flips (", Nsim, " simulations)",
                     sep=""))
}
