P <- function(height) {
  pnorm(height, mean=180, sd=6.5)
}

P(180) - P(170)
