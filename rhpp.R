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

other.hpp <- function(lambda,stoptime) {
  expectednumber <- stoptime*lambda
  Nsim <- 3*expectednumber
  timesbetween <- rexp(Nsim,lambda) # Simulate interarrival times
  timesto <- cumsum(timesbetween)   # Calculate arrival times
  timesto <- timesto[timesto<stoptime] # Dischard the times larger than stoptime
  return (timesto)
#  Nevents <- length(timesto) # Count the number of events
#  plot(timesto,1:Nevents,type="s",xlab = "arrival time",
#       ylab = "Event number",lwd=1.5,ylim=c(0,Nevents))
#  points(timesto,rep(0,Nevents),pch=21,bg="red")
}

rhpp <- function(lambda, start, stop, multiplier=3) {
  # Number of simulations required
  expected <- stop * lambda
  n <- multiplier * expected
  
  # Arrival times
  w <- start + cumsum(rexp(n, lambda))
  
  # Filter out those larger than t.max
  w <- w[w <= stop]
}

plotRHPP <- function(lambda, t.max) {
  w <- rhpp(lambda, t.max)
  plot(w, 1:length(w), type="s", ylim=c(0, length(w)), lwd=1.5, 
       xlab="Arrival time", ylab="Event number")
  points(w, rep(0, length(w)), pch=21, bg="red")
}


inverse <- function(w) {
  (10^(-5/7)) * (w^(5/7))
}

lfunc <- function(t) {
  10*t^(7/5)
}

ofunc <- function(t) {
  14*t^0.4
}

plotRNHPP <- function(a, b, lambda.fun, inverse.fun) {
  #w <- cumsum(rexp(1000, rate=1))
  #w <- cumsum(rpois(1000, lambda=1))
  w <- rhpp(lambda=1, start=lambda.fun(a), stop=lambda.fun(b), multiplier=1)
  s <- a + inverse.fun(w)
  s <- s[s <= b]
  plot(s, 1:length(s), type="s", ylim=c(0, 120), lwd=1.5,
       xlab="Arrival time", ylab="Event number", main="xMy RNHPP")
  points(s, rep(0, length(s)), pch=21, bg="red")
}

plotRNHPP.old <- function(a, b, intensity.fun, inverse.fun) {
  w <- intensity.fun(a) + rhpp(lambda=1, start=intensity.fun(a),
                               stop=intensity.fun(b), multiplier=3)
  #w <- lfunc(a) + other.hpp(lambda=1, stoptime=intensity.fun(b))
  s <- inverse(w)

  plot(s, 1:length(s), type="s", ylim=c(0, 100), lwd=1.5,
       xlab="Arrival time", ylab="Event number", main="My RNHPP")
  points(s, rep(0, length(s)), pch=21, bg="red")
}

#par(mfrow=c(3, 1))
#plotHPP(lambda=1,stoptime=5)
#plotRHPP(lambda=1, t.max=5)

par(mfrow=c(2,1))
plotRNHPP(0, 5, lfunc, inverse)

# Function for simulating arrival times for a NHPP between a and b using thinning
simtNHPP <- function(a,b,lambdamax,lambdafunc){
  # Simple check that a not too small lambdamax is set
  if(max(lambdafunc(seq(a,b,length.out = 100)))>lambdamax)
    stop("lambdamax is smaller than max of the lambdafunction")
  # First simulate HPP with intensity lambdamax on a to b
  expectednumber <- (b-a)*lambdamax
  Nsim <- 3*expectednumber  # Simulate more than the expected number to be certain to exceed stoptime
  timesbetween <- rexp(Nsim,lambdamax) # Simulate interarrival times
  timesto <- a+cumsum(timesbetween)   # Calculate arrival times starting at a
  timesto <- timesto[timesto<b] # Dischard the times larger than b
  Nevents <- length(timesto) # Count the number of events
  # Next do the thinning. Only keep the times where u<lambda(s)/lambdamax
  U <- runif(Nevents)
  timesto <- timesto[U<lambdafunc(timesto)/lambdamax]
  timesto  # Return the remaining times
}

# Plot the intensity function
#tvec <- seq(0,5,by=0.01)
#plot(tvec,lfunc(tvec),type="l",ylim=c(0,100))

# Generate data with the traffic intensity and plot them
NHPPtimes <- simtNHPP(a=0,b=5,lambdamax=100,lambdafunc=ofunc)
NHPPtimes <- NHPPtimes[NHPPtimes < 5]
plot(NHPPtimes,1:length(NHPPtimes),type="s",xlab = "time",
     ylab = "Event number",lwd=1.5, ylim=c(0,120))
points(NHPPtimes,rep(0,length(NHPPtimes)),pch=21,bg="red")
# Rerun the lines above several times