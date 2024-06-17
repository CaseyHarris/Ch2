model {
  mu <- X %*% b ## expected response
  for (i in 1:n) { y[i] ~ x[i]/sqrt(s[i]) }
  for (i in 1:n) { x[i] ~ dnorm(mu[i], tau) }
  for (i in 1:n) { s[i] ~ dgamma(k/2, k/2) } ## response 
  tau ~ dgamma(0.05, 0.005) ## precision parameter prior 
  k ~ dgamma(0.05, 0.005)
  ## Parametric effect priors CHECK tau=1/95^2 is appropriate!
  for (i in 1:1) { b[i] ~ dnorm(0, 0.00011) }
  ## prior for s(log_flow)... 
  K1 <- S1[1:3,1:3] * lambda[1]  + S1[1:3,4:6] * lambda[2]
  b[2:4] ~ dmnorm(zero[2:4],K1) 
  ## smoothing parameter priors CHECK...
  for (i in 1:2) {
    lambda[i] ~ dgamma(0.05, 0.005)
    rho[i] <- log(lambda[i])
  }
}