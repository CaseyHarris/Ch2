model {
  eta <- X %*% b ## linear predictor
  for (i in 1:n) { mu[i] <-  exp(eta[i]) } ## expected response
  for (i in 1:n) { y[i] ~ dgamma(r,r/mu[i]) } ## response 
  r ~ dgamma(.05,.005) ## scale parameter prior 
  scale <- 1/r ## convert r to standard GLM scale
  ## Parametric effect priors CHECK tau=1/13^2 is appropriate!
  for (i in 1:1) { b[i] ~ dnorm(0,0.0058) }
  ## prior for s(Q_cfs_log_round)... 
  K1 <- S1[1:3,1:3] * lambda[1]  + S1[1:3,4:6] * lambda[2]
  b[2:4] ~ dmnorm(zero[2:4],K1) 
  ## smoothing parameter priors CHECK...
  for (i in 1:2) {
    lambda[i] ~ dgamma(.05,.005)
    rho[i] <- log(lambda[i])
  }
}