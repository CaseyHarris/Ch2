library(tidyverse)
library(lubridate)
library(mgcv)
library(jagsUI)
library(gratia)

#How does water quality change with streamflow, or streamflow and temperature?
#What is the probability of water quality exceeding relevant thresholds?
#Report future distributions of water quality and uncertainty

#Using a generalized additive model for flexible relationships
#Using MCMC/Bayesian analysis to provide credible intervals (interval that contains X% of values)
#for interpretability rather than frequentist confidence intervals for making decisions
#also useful for hierarchical models, constraining outcomes, examining the posterior in various ways

#Actual data
data0 <- read.csv("01_Data/actual data.csv") #total organic carbon vs streamflow
data0 <- data0 %>%
  mutate(date = as.Date(date),
         row = row_number(),
         log_flow = log10(flow),
         yday = yday(date)) %>%
  select(date, yday, flow, log_flow, wq, log_wq)

ggplot(data0, aes(wq)) +
  geom_histogram()

ggplot(data0, aes(flow)) +
  geom_histogram(bins=100) 

ggplot(data0, aes(flow, wq)) +
  geom_point()

#Use a GAM to simulate some data
mod0 <- gam(wq ~ s(log_flow, bs="tp", k=4), family="scat", data=data0) #scat - student's t distribution

pred_mean <- predict.gam(mod0, newdata=data0)
data0$pred_mean <- c(pred_mean)

data_for_sim <- data0 %>%
  select(log_flow)

sim_wq <- simulate(mod0, nsim=1000, data=data0)
#simulate() gets the predicted values and then randomly draws from their probability distribution, could read docs to find out how

sim_summary <- data.frame()
for (i in 1:132) {
  dat <- sim_wq[i,]
  sim_low <- quantile(dat, .025)
  sim_high <- quantile(dat, .975)
  sim_mean <- mean(dat)
  sim_median <- median(dat)
  
  sim_summary_new <- data.frame(sim_low, sim_high, sim_mean, sim_median)
  sim_summary <- sim_summary %>%
    bind_rows(sim_summary_new)
}

data1 <- data0 %>%
  mutate(sim_wq = sim_wq[,1]) %>%
  bind_cols(sim_summary)

ggplot(data1, aes(flow, wq)) +
  geom_errorbar(data=data1, aes(ymin=sim_low, ymax=sim_high)) +
  geom_point(shape=1, size=2) +
  geom_point(data=data1, aes(flow, sim_wq), color="red") +
  geom_point(data=data1, aes(flow, pred_mean), color="orange")
  #geom_point(data=data1, aes(flow, sim_mean), color="blue")

#Issues with this simulation of data -- 
#the simulated data don't overlap with the actual data very well
#the credible intervals encompass the simulated data but not the actual data very well

################################################################################

#Now for Bayesian version
tmp_jags_code <- "02_Scripts/tmp_jags_code 2024-07-18 unedited.R" #where the jags code file will be written
tmp_model_obs <- jagam(wq ~ s(log_flow, bs="tp", k=4), data=data1, file=tmp_jags_code, family=gaussian(link="identity"))

#have to change jags file to scat by typing, JAGAM cannot do it
#not sure if all the changes I've made are OK
#letting JAGS estimate s and z, then will estimate y's from those
#am I estimating the y's correctly? (see below)

#Set parameters to monitor
#when I include y's in parameters to monitor, they all end up being exactly the same, don't know why
params = c("b", "tau", "k", "mu", "S", "z") #b are the parameters, tau is precision, mu is expected values, y is estimated values

#Run model
jags_code <- "02_Scripts/tmp_jags_code 2024-06-13 edited6.R"
mod1 <- jags(model.file = jags_code,
                parameters.to.save = params,
                data = tmp_model_obs$jags.data, #makes the design matrix for me?
                #inits = tmp_model_obs$jags.ini,
                n.chains = 3, #number of chains
                n.adapt = 1000,
                n.burnin = 2000, #number of iterations to discard as burn-in
                n.iter = 4500, #number of iterations
                n.thin = 25, #interval to thin
                DIC = TRUE)

#mod1$samples and mod1$sims.list are the same, just arranged differently
mcmc <- do.call('rbind', mod1$samples)

#mu ends up the same as using tmp_model$jags.data$X %*% t(mcmc[,1:6])
mcmc_df <- data.frame(mcmc)
mcmc_df0 <- mcmc_df[,1:6]
mcmc_df1 <- mcmc_df[,7:138]
mcmc_df2 <- mcmc_df[,139:270]
mcmc_df3 <- mcmc_df[,271:402]
mcmc_longer1 <- mcmc_df0 %>%
  bind_cols(mcmc_df1) %>%
  pivot_longer(cols=7:138) %>%
  mutate(flow = rep(data0$flow, 300)) %>%
  mutate(mu = value) %>%
  select(-name, -value)
mcmc_longer2 <- mcmc_df0 %>%
  bind_cols(mcmc_df2) %>%
  pivot_longer(cols=7:138) %>%
  mutate(flow = rep(data0$flow, 300)) %>%
  mutate(S = value) %>%
  select(-name, -value)
mcmc_longer3 <- mcmc_df0 %>%
  bind_cols(mcmc_df3) %>%
  pivot_longer(cols=7:138) %>%
  mutate(flow = rep(data0$flow, 300)) %>%
  mutate(z = value) %>%
  select(-name, -value)
mcmc_longer <- mcmc_longer1 %>%
  mutate(S = mcmc_longer2$S,
         z = mcmc_longer3$z) %>%
  mutate(y = z/sqrt(S)) #this the the line where I am estimating y's, not sure if correct

mcmc_longer_summary <- mcmc_longer %>%
  group_by(flow) %>%
  summarise(y_025 = quantile(y, .025),
            y_975 = quantile(y, .975),
            y_50 = quantile(y, .5),
            y_mean = mean(y))

unique(mcmc_longer$flow)
ggplot(mcmc_longer, aes(y, group=flow)) +
  geom_density() #some y values (supposed to be total organic carbon) are enormous
#7:138 mu
#139:270 S
#271:402 z

##ask why I simulate ridiculously high values
##can we control how high it goes
##but something else is probably wrong

ggplot(mcmc_longer_summary, aes(flow, y_50)) +
  geom_errorbar(data=mcmc_longer_summary, aes(flow, ymin=y_025, ymax=y_975), color="black") +
  geom_point(data=data1, aes(flow, wq), color="black", shape=1) + #original wq
  #geom_point(color='red', shape=1) + #median modeled y's (y_50)
  geom_point(data=mcmc_longer[1:132,], aes(flow, y), color="red") +
  #geom_point(data=mcmc_longer, aes(flow, mu), color="orange") + #expected mean
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal()
rm(mcmc_longer)
#mu  are good, but y values have much bigger spread than actual data