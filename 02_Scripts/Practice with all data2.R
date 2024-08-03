library(mgcv)
library(gratia)
library(tidyverse)
library(jagsUI)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
flow_names <- list.files("01_Data/Orig wq and flow/FLOW")

setdiff(wq_names, flow_names)
setdiff(flow_names, wq_names)

flow_Jason <- read.csv("01_Data/Chang data business as usual.csv")

summary <- data.frame()
i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names) {
  
  title <- gsub(".csv", "", i) #will be plot title
  
  wq <- read.csv(paste0("01_Data/Orig wq and flow/WQ/", i, sep="")) #water quality data
  flow <- read.csv(paste0("01_Data/Orig wq and flow/FLOW/", i, sep="")) #streamflow data
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen) %>%
    filter(Date>=as.Date("2010-01-01")) #only using 2010 onward so change over time won't be as much of an issue
  
  flow0 <- flow %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Q)
  
  wq_flow0 <- flow0 %>%
    left_join(wq0) %>%
    mutate(i = i) %>%
    filter(Date>=as.Date("2010-01-01")) #only using 2010 onward so change over time won't be as much of an issue
  
  summary0 <- wq_flow0 %>%
    group_by(Uncen) %>%
    summarise(n = n())
  
  summary1 <- data.frame("i" = i, "Uncen_1" = summary0$n[summary0$Uncen==1 & !is.na(summary0$Uncen)], "WQ_length" = length(wq0$Date))
  summary <- summary %>%
    bind_rows(summary1) #this table will show how many of the measurements are censored (below lab detection limit)

  ##########################################################
  #Bayesian version
  wq_flow1 <- wq_flow0 %>%
    mutate(wq_orig = ConcAve,
           wq_cal = ConcAve) %>%
    select(i, Date, Q, wq_orig, wq_cal)
  
  #Splitting the dataset
  n <- nrow(wq_flow1)
  set.seed(59)
  split <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.75, 0.25))
  
  cal <- wq_flow1[split, ]
  val <- wq_flow1[!split, ]
  
  val <- val %>%
    mutate(wq_cal = NA)
  
  cal_val_df <- cal %>%
    bind_rows(val)
  
  tmp_jags_code <- "02_Scripts/tmp_jags_code 2024-08-02 unedited.R" #where the jags code file will be written
  set.seed(90)
  tmp_model_obs_des <- jagam(wq_cal ~ s(Q, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=gaussian, na.action=na.pass) #gives me the design matrix I need
  set.seed(90)
  tmp_model_obs <- jagam(wq_cal ~ s(Q, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=Gamma(link="log")) #give me the jags script I need
  #design matrices for tmp_model_obs and tmp_model_des are identical with na.action=na.omit,
  #but when I use na.pass it differs slightly (why?)
  own_design_matrix <- tmp_model_obs_des$jags.data
  
  #add something like this to JAGS code to estimate y values:
  # for (i in 1:n) {
  #   y_est[i] <- dgamma(r, r/mu[i])
  # }
  
  #Set parameters to monitor
  params = c("b", "eta", "mu", "r", "scale") #include y or y_est when working 
  #b are the GAM parameters, eta is needed for the log link, mu is expected values of mean water quality, 
  #r and scale describe the gamma distribution
  #y is response values of water quality

  #Run model
  jags_code <- "02_Scripts/tmp_jags_code 2024-08-02 unedited.R"
  #jags_code <- "02_Scripts/tmp_jags_code 2024-08-02 v1.R"
  set.seed(row_number(wq_names[wq_names==i]))
  mod1 <- jags(model.file = jags_code,
               parameters.to.save = params,
               data = own_design_matrix, #includes NAs
               #inits = tmp_model_obs$jags.ini, #error message when I include inits
               n.chains = 3, #number of chains
               n.adapt = 1000,
               n.burnin = 2000, #number of iterations to discard as burn-in
               n.iter = 4500, #number of iterations
               n.thin = 25) #interval to thin

  #mod1$samples and mod1$sims.list are the same, just arranged differently
  mcmc <- do.call('rbind', mod1$samples)

  # test <- own_design_matrix$X %*% t(mcmc[,1:4]) #I think this would be another way of getting eta values,
  # test <- exp(test) #mu values
  # then do below to get y values
  
  n_obs <- length(cal_val_df$Date)
  mcmc_df <- data.frame(mcmc)
  mcmc_df0 <- mcmc_df[,1:4] #b
  mcmc_df1 <- mcmc_df[,5:(n_obs+4)] #eta
  mcmc_df2 <- mcmc_df[,(n_obs+5):(n_obs*2+4)] #mu
  mcmc_df3 <- mcmc_df[,n_obs*2+5] #r
  mcmc_df4 <- mcmc_df[,n_obs*2+6] #scale
  #mcmc_df5 <- mcmc_df[,(n_obs*2+7):(n_obs*3+6)] #y_est  
  
  #Rearranging the posterior
  mcmc_longer1 <- mcmc_df0 %>%
    mutate(r = mcmc_df3,
           scale = mcmc_df4) %>%
    bind_cols(mcmc_df1) %>%
    pivot_longer(cols=7:(n_obs+6)) %>%
    mutate(flow = rep(cal_val_df$Q, 300)) %>%
    mutate(eta = value) %>%
    select(-name, -value)
  mcmc_longer2 <- mcmc_df0 %>%
    mutate(r = mcmc_df3,
           scale = mcmc_df4) %>%
    bind_cols(mcmc_df2) %>%
    pivot_longer(cols=7:(n_obs+6)) %>%
    mutate(flow = rep(cal_val_df$Q, 300)) %>%
    mutate(mu = value) %>%
    select(-name, -value)
  # mcmc_longer3 <- mcmc_df0 %>%
  #   mutate(r = mcmc_df3,
  #          scale = mcmc_df4) %>%
  #   bind_cols(mcmc_df5) %>%
  #   pivot_longer(cols=7:(n_obs+6)) %>%
  #   mutate(flow = rep(cal1$Q, 300)) %>%
  #   mutate(y_est = value) %>%
  #   select(-name, -value)
    
  mcmc_longer <- mcmc_longer1 %>%
    mutate(mu = mcmc_longer2$mu) #,
    #       y_est = mcmc_longer3$y_est)

  #Getting estimated y values
  k=1
  mcmc_longer$y <- NA
  for (k in 1:length(mcmc_longer$b.1.)) {

    set.seed(k)
    y = rgamma(1, mcmc_longer$r[k], mcmc_longer$r[k]/mcmc_longer$mu[k])
    mcmc_longer$y[k] <- y
    
    rem <- k%%100000
    if (rem==0) {
      write.csv(mcmc_longer, "01_Data/mcmc results.csv", row.names=FALSE)
    }

  } # seems like a fine way to get y's, but I wouldn't do this if my y's were working in JAGS
  #takes a long time for bigger datasets (~30 minutes for 10 years of flow data)

  #Getting 95% credible intervals
  mcmc_longer_res <- mcmc_longer %>%
    mutate(label = as.character(rep(1:n_obs, 300)))
  
  mcmc_summary <- mcmc_longer_res %>%
    group_by(label, flow) %>%
    summarise(mu_mean = mean(mu),
              mu_025 = quantile(mu, .025),
              mu_975 = quantile(mu, .975),
              y_mean = mean(y),
              y_025 = quantile(y, .025),
              y_975 = quantile(y, .975)) 
  
  cal_val_df <- cal_val_df %>%
    mutate(label = as.character(1:n_obs)) %>%
    mutate(cal_or_val = case_when(!is.na(wq) ~ "Model calibration",
                                  is.na(wq) ~ "Model validation",
                                  TRUE ~ NA_character_))
  
  mcmc_summary1 <- mcmc_summary %>%
    left_join(cal_val_df, by="label")
  
  mcmc_longer_res1 <- mcmc_longer_res %>%
    left_join(cal_val_df, by="label") %>%
    mutate(flow = Q)
  
  mcmc_plot <- mcmc_longer_res1[((100*length(cal_val_df$wq)+1)):(101*length(cal_val_df$wq)),] #the 100th simulation, could choose anything
  
  ggplot(mcmc_summary1, aes(flow, wq_orig, color=cal_or_val)) +
    geom_line(aes(flow, y_025)) +
    geom_line(aes(flow, y_975)) +
    geom_line(aes(flow, mu_mean), linetype="dashed") +
    geom_line(aes(flow, mu_025)) +
    geom_line(aes(flow, mu_975)) +
    geom_point(data=mcmc_plot, aes(flow, y), shape=1) +
    geom_point(shape=1, aes(color="Observed values")) +
    ggtitle(paste0("JAGS version: ", title, sep="")) +
    ylab("Water quality (units)") +
    xlab("Flow (m^3/s)") +
    scale_color_manual(values=c("Model calibration"="green", "Model validation"="red", "Observed values"="black")) +
    theme_bw() +
    theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", title, " jags.png", sep=""), width=6, height=3.75)
  
  # #modifying the code to use rjags and then 
  # #using coda() or coda.samples() might help me get estimated y values
  # #examples on cran NOT WORKING YET for validation dataset
  # require(rjags)
  # tmp_model_obs$jags.ini$.RNG.name <- "base::Mersenne-Twister" ## setting RNG
  # tmp_model_obs$jags.ini$.RNG.seed <- 6 ## how to set RNG seed
  # mod2 <-jags.model(jags_code, data=tmp_model_obs$jags.data,
  #                   inits=tmp_model_obs$jags.ini, n.chains=3)
  # sam <- jags.samples(mod2, c("b", "eta", "mu", "r", "scale"),
  #                     n.adapt=1000, n.burnin=2000, n.iter=4500, thin=25)
  # jam <- sim2jam(sam, tmp_model_obs$pregam)
  # plot(jam) #?
  # jam
  # pred_y <- predict(jam, newdata=val) #gives me eta values?
  # 
  # mcmc_longer$wq_obs <- rep(cal$wq, 300) #make sure order is not messed up or join more carefully
  
  #NSE=1 perfect model, NSE=0 as good as using the mean, NSE<0 worse than using the mean
  nse_bayes <- 1 - (sum((mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$wq_orig - mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$y_est)^2) / sum((mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$wq_orig - mean(mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$y_est))^2))
  nse_bayes_alt <- 1 - (sum(abs(mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$wq_orig - mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$y_est)) / sum(abs(mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$wq_orig - mean(mcmc_plot[mcmc_plot$cal_or_val=="Model validation",]$y_est))))
  #alt version may be less sensitive to outliers
  
  ggplot(mcmc_plot, aes(wq_orig, y_est, color=cal_or_val)) +
    geom_point(shape=1) + 
    geom_abline(aes(slope=1, intercept=0), linetype="dashed") +
    ggtitle(paste0("JAGS version: ", title, sep=""), subtitle=paste0("NSE = ", round(nse_bayes, 2), " and NSE_alt = ", round(nse_bayes_alt, 2), sep="")) +
    ylab("Simulated water quality") +
    xlab("Observed water quality") +
    scale_color_manual(values=c("Model calibration"="green", "Model validation"="red")) +
    theme_bw() +
    theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", title, " jags nse.png", sep=""), width=6, height=3.75)
  
  #GAM only version
  mod_gam <- gam(wq ~ s(Q, bs="tp", k=4), family=Gamma(link="log"), data=cal_val_df)
  
  pred_mean <- predict.gam(mod_gam, newdata=cal_val_df)
  cal_val_df$pred_mean <- c(pred_mean)
  
  sim_wq <- simulate(mod_gam, nsim=1000, data=cal_val_df)
  #simulate() gets the predicted values and then randomly draws from their probability distribution
  
  j=1
  sim_summary <- data.frame()
  n_obs_gam = length(cal_val_df$Date)
  for (j in 1:n_obs_gam) {
    dat <- sim_wq[j,]
    sim_low <- quantile(dat, .025)
    sim_high <- quantile(dat, .975)
    sim_mean <- mean(dat)
    sim_median <- median(dat)
    sim_val <- sim_wq[j,100] #the 100th set of simulated y values, could pick anything
    
    sim_summary_new <- data.frame(sim_low, sim_high, sim_mean, sim_median, sim_val)
    sim_summary <- sim_summary %>%
      bind_rows(sim_summary_new)
    
  }

  sim_summary <- sim_summary %>%
    bind_cols(cal_val_df)

  ggplot(sim_summary, aes(Q, wq_orig, color=cal_or_val)) +
    geom_point(shape=1, aes(color="Observed values")) + 
    geom_line(aes(Q, sim_low)) +
    geom_line(aes(Q, sim_high)) +
    geom_line(aes(Q, sim_mean), linetype="dashed") +
    geom_point(shape=1, aes(Q, sim_val)) +
    ggtitle(paste0("GAM version: ", title, sep="")) +
    ylab("Water quality (units)") +
    xlab("Flow (m^3/s)") +
    theme_bw() +
    scale_color_manual(values=c("Model calibration"="green", "Model validation"="red", 
                       "Observed values"="black")) +
    theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", title, " gam.png", sep=""), width=6, height=3.75)
  
  nse <- 1 - (sum((sim_summary$wq_orig - sim_summary$sim_val)^2) / sum((sim_summary$wq_orig - mean(sim_summary$wq_orig))^2)) 
  nse_alt <- 1 - (sum(abs(sim_summary$wq_orig - sim_summary$sim_val)) / sum(abs(sim_summary$wq_orig - mean(sim_summary$wq_orig))))
  #there's probably a better way to calculate this (average NSE?), this uses only one set of simulated values
  
  ggplot(sim_summary, aes(wq_orig, sim_val, color=cal_or_val)) +
    geom_point(shape=1) + 
    geom_abline(slope=1, intercept=0, linetype="dashed") +
    ggtitle(paste0("GAM version: ", title, sep=""), subtitle=paste0("NSE = ", round(nse, 2), " and NSE_alt = ", round(nse_alt, 2), sep="")) +
    ylab("Observed water quality") +
    xlab("Simulated water quality") +
    scale_color_manual(values=c("Model calibration"="green", "Model validation"="red")) +
    theme_bw() +
    theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  ggsave(paste0("01_Data/Orig wq and flow/Initial C-D relationships/", title, " gam nse.png", sep=""), width=6, height=3.75)
  
}
