# Lecture code example modified by Christian Stigen

# Reset environment for interactive runs
rm(list=ls())

# Simulate the number of heads in k throws of a dice repeated Nsim times. We
# let 0 correspond to tail and 1 correspond to head. The sample()-command below
# draw a 0 or 1 k times, and then sum() will summarize the number of heads.
simheads <- function(Nsim,k) {
  nheads <- vector(length=Nsim)
  for(i in 1:Nsim)
    nheads[i] <- sum(sample(0:1,size=k,replace=TRUE))
  return(nheads)
}


# We want to count how many heads we get in k=5 coin flips. We do this entire
# process Nsim times. Below, we do that, but increase the number of
# simulations (Nsim) and plot the results.

# Draw several graphs below each other
par(mfrow=c(2,1))

for (Nsim in c(10,10000)) {
  k <- 5
  nheadsim <- simheads(Nsim=Nsim,k=k)
  relfreq <- table(nheadsim)/Nsim   # Calculate relative frequency for each outcome
  barplot(relfreq,
          ylab="Relative frequency",
          xlab=paste("Heads in ", k, " coin flips (", Nsim, " simulations)",
                     sep=""))
}
