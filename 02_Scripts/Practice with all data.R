library(mgcv)
library(gratia)
library(tidyverse)
library(jagsUI)

#Preparing data for analysis

#2010 - 2019
#2014 - 2023

#Future streamflow
#2030 - 2060
#2070 - 2100

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
flow_names <- list.files("01_Data/Orig wq and flow/FLOW")

setdiff(wq_names, flow_names)
setdiff(flow_names, wq_names)

summary <- data.frame()
wq_flow_all <- data.frame()
wq_flow_obs <- data.frame()
i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names) {
  
  wq <- read.csv(paste0("01_Data/Orig wq and flow/WQ/", i, sep=""))
  flow <- read.csv(paste0("01_Data/Orig wq and flow/FLOW/", i, sep=""))
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen)
  
  flow0 <- flow %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Q)
  
  wq_flow0 <- flow0 %>%
    left_join(wq0) %>%
    mutate(i = i)
  
  wq_flow_all <- wq_flow_all %>%
    bind_rows(wq_flow0)
  
  wq_flow1 <- wq_flow0 %>%
    filter(!is.na(ConcAve)) %>%
    mutate(i = i)
  
  wq_flow_obs <- wq_flow_obs %>%
    bind_rows(wq_flow1)
    
  summary0 <- wq_flow0 %>%
    group_by(Uncen) %>%
    summarise(n = n())
  
  summary1 <- data.frame("i" = i, "Uncen_1" = summary0$n[summary0$Uncen==1 & !is.na(summary0$Uncen)], "WQ_length" = length(wq0$Date))
  summary <- summary %>%
    bind_rows(summary1)
  
  ############################
  wq_flow2 <- wq_flow1 %>%
    mutate(wq = ConcAve,
           log_flow = log10(Q))
  
  #Now for Bayesian version
  tmp_jags_code <- "02_Scripts/tmp_jags_code 2024-07-18 unedited.R" #where the jags code file will be written
  tmp_model_obs <- jagam(wq ~ s(log_flow, bs="tp", k=4), data=wq_flow2, file=tmp_jags_code, family=Gamma(link="log"))
  
  #have to change jags file to scat by typing, JAGAM cannot do it
  #not sure if all the changes I've made are OK
  #letting JAGS estimate s and z, then will estimate y's from those
  #am I estimating the y's correctly? (see below)
  
  #Set parameters to monitor
  #when I include y's in parameters to monitor, they all end up being exactly the same, don't know why
  #params = c("b", "tau", "k", "mu", "S", "z") #b are the parameters, tau is precision, mu is expected values, y is estimated values
  params = c("b", "eta", "mu", "r", "scale") #b are the parameters, tau is precision, mu is expected values, y is estimated values
  
  #Run model
  jags_code <- "02_Scripts/tmp_jags_code 2024-06-13 edited9.R"
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
  # mcmc_df <- data.frame(mcmc)
  # mcmc_df0 <- mcmc_df[,1:6]
  # mcmc_df1 <- mcmc_df[,7:138]
  # mcmc_df2 <- mcmc_df[,139:270]
  # mcmc_df3 <- mcmc_df[,271:402]
  
  n_obs <- length(wq_flow2$Date)
  mcmc_df <- data.frame(mcmc)
  mcmc_df0 <- mcmc_df[,1:4] #b
  mcmc_df1 <- mcmc_df[,5:(n_obs+4)] #eta
  mcmc_df2 <- mcmc_df[,(n_obs+5):(n_obs*2+4)] #mu
  mcmc_df3 <- mcmc_df[,n_obs*2+5]
  mcmc_df4 <- mcmc_df[,n_obs*2+6]
  
  mcmc_longer1 <- mcmc_df0 %>%
    mutate(r = mcmc_df3,
           scale = mcmc_df4) %>%
    bind_cols(mcmc_df1) %>%
    pivot_longer(cols=7:(n_obs+6)) %>%
    mutate(flow = rep(wq_flow2$log_flow, 300)) %>%
    mutate(eta = value) %>%
    select(-name, -value)
  mcmc_longer2 <- mcmc_df0 %>%
    mutate(r = mcmc_df3,
           scale = mcmc_df4) %>%
    bind_cols(mcmc_df2) %>%
    pivot_longer(cols=7:(n_obs+6)) %>%
    mutate(flow = rep(wq_flow2$log_flow, 300)) %>%
    mutate(mu = value) %>%
    select(-name, -value)
  mcmc_longer <- mcmc_longer1 %>%
    mutate(mu = mcmc_longer2$mu)
  
  k=4
  mcmc_longer$y_est <- NA
  for (k in 1:length(mcmc_longer$b.1.)) {
    
    y_est = rgamma(1, mcmc_longer$r[k], mcmc_longer$r[k]/mcmc_longer$mu[k])
    mcmc_longer$y_est[k] <- y_est
    
  }
  
  mcmc_summary <- mcmc_longer %>%
    mutate(label = as.character(rep(1:n_obs, 300))) %>%
    group_by(label, flow) %>%
    summarise(mu_mean = mean(mu),
              mu_025 = quantile(mu, .025),
              mu_975 = quantile(mu, .975),
              y_mean = mean(y_est),
              y_025 = quantile(y_est, .025),
              y_975 = quantile(y_est, .975))
  
  ggplot(mcmc_longer, aes(10^flow, mu)) +
    #geom_point(aes(10^flow, y_est), shape=1, color="green") +
    geom_line(data=mcmc_summary, aes(10^flow, y_mean), linetype="dashed", color="green") +
    geom_line(data=mcmc_summary, aes(10^flow, y_025), color="green") +
    geom_line(data=mcmc_summary, aes(10^flow, y_975), color="green") +
    #geom_point(shape=1, color="blue") +
    geom_line(data=mcmc_summary, aes(10^flow, mu_mean), linetype="dashed", color="blue") +
    geom_line(data=mcmc_summary, aes(10^flow, mu_025), color="blue") +
    geom_line(data=mcmc_summary, aes(10^flow, mu_975), color="blue") +
    geom_point(data=wq_flow2, aes(10^log_flow, ConcAve), shape=1) +
    ggtitle(i) +
    ylab("Water quality characteristic (units of measure)") +
    xlab("Flow (cubic meters per second)") +
    theme_minimal()
  ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", i, ".png", sep=""))
  
  # mod0 <- gam(wq ~ s(log_flow, bs="tp", k=4), data=wq_flow2) #family="scat" - student's t distribution
  # 
  # pred_mean <- predict.gam(mod0, newdata=wq_flow2)
  # wq_flow2$pred_mean <- c(pred_mean)
  # 
  # data_for_sim <- wq_flow2 %>%
  #   select(log_flow)
  # 
  # sim_wq <- simulate(mod0, nsim=1000, data=wq_flow2)
  # #simulate() gets the predicted values and then randomly draws from their probability distribution
  # 
  # sim_summary <- data.frame()
  # n_obs = length(wq_flow2$Date)
  # for (j in 1:n_obs) {
  #   dat <- sim_wq[j,]
  #   sim_low <- quantile(dat, .025)
  #   sim_high <- quantile(dat, .975)
  #   sim_mean <- mean(dat)
  #   sim_median <- median(dat)
  #   
  #   sim_summary_new <- data.frame(sim_low, sim_high, sim_mean, sim_median)
  #   sim_summary <- sim_summary %>%
  #     bind_rows(sim_summary_new)
  # }
  # 
  # wq_flow3 <- wq_flow2 %>%
  #   mutate(sim_wq = sim_wq[,1]) %>%
  #   bind_cols(sim_summary)
  # 
  # ggplot(wq_flow3, aes(10^log_flow, wq)) +
  #   geom_errorbar(data=wq_flow3, aes(ymin=sim_low, ymax=sim_high)) + #simulated range
  #   geom_point(shape=1, size=2) + #observed
  #   geom_point(data=wq_flow3, aes(10^log_flow, sim_wq), color="red") + #simulated
  #   geom_point(data=wq_flow3, aes(10^log_flow, pred_mean), color="orange") + #expected mean
  #   ggtitle(i)
  # ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", i, " scat.png", sep=""))
  # 
}

# summary <- summary %>%
#   mutate(diff = WQ_length - Uncen_1)

