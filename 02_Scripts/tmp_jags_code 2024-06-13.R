model {
  eta <- X %*% b ## linear predictor
  for (i in 1:n) { mu[i] <-  exp(eta[i]) } ## expected response
  for (i in 1:n) { y[i] ~ dnorm(mu[i],tau) } ## response 
  scale <- 1/tau ## convert tau to standard GLM scale
  tau ~ dgamma(.05,.005) ## precision parameter prior 
  ## Parametric effect priors CHECK tau=1/29^2 is appropriate!
  for (i in 1:1) { b[i] ~ dnorm(0,0.0012) }
  ## prior for s(log_flow)... 
  K1 <- S1[1:3,1:3] * lambda[1]  + S1[1:3,4:6] * lambda[2]
  b[2:4] ~ dmnorm(zero[2:4],K1) 
  ## smoothing parameter priors CHECK...
  for (i in 1:2) {
    lambda[i] ~ dgamma(.05,.005)
    rho[i] <- log(lambda[i])
  }
}