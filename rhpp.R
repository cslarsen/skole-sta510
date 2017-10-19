plotHPP <- function(lambda,stoptime) {
  expectednumber <- stoptime*lambda
  Nsim <- 3*expectednumber
  timesbetween <- rexp(Nsim,lambda) # Simulate interarrival times
  timesto <- cumsum(timesbetween)   # Calculate arrival times
  timesto <- timesto[timesto<stoptime] # Dischard the times larger than stoptime
  Nevents <- length(timesto) # Count the number of events
  plot(timesto,1:Nevents,type="s",xlab = "arrival time",
       ylab = "Event number",lwd=1.5,ylim=c(0,Nevents))
  points(timesto,rep(0,Nevents),pch=21,bg="red")
}

rhpp <- function(lambda, t.max, multiplier=3) {
  # Number of simulations required
  n <- t.max * lambda
  n <- multiplier * n
  
  # Arrival times
  w <- cumsum(rexp(n, lambda))
  
  # Filter out those larger than t.max
  w <- w[w <= t.max]
}

plotRHPP <- function(lambda, t.max) {
  w <- rhpp(lambda, t.max)
  plot(w, 1:length(w), type="s", ylim=c(0, length(w)), lwd=1.5, 
       xlab="Arrival time", ylab="Event number")
  points(w, rep(0, length(w)), pch=21, bg="red")
}

plotRNHPP <- function(inverse, t.max) {
  w <- rhpp(lambda=1, t.max=10*t.max^(7/5), multiplier=3)
  s <- inverse(w)
  plot(s, 1:length(s), type="s", ylim=c(0, length(s)), lwd=1.5,
       xlab="Arrival time", ylab="Event number")
}

par(mfrow=c(3, 1))
plotHPP(lambda=1,stoptime=5)
plotRHPP(lambda=1, t.max=5)

inverse <- function(w) {
  10^(-5/7) * w^(5/7)  
}

plotRNHPP(inverse=inverse, t.max=5)