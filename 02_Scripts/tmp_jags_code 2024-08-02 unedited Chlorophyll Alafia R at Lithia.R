model {
  eta <- X %*% b ## linear predictor
  for (i in 1:n) { mu[i] <-  exp(eta[i]) } ## expected response
  for (i in 1:n) { 