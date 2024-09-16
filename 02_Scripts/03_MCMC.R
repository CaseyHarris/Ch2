library(mgcv)
#library(gratia)
library(tidyverse)
library(jagsUI)
library(data.table)

#use retrospective or the climate model we're talking about to compare time periods
#nldas would tell us how good the model is
#ch1 told us about imp. of land management
#ch2 holds land management const, look at just streamflow
#could compare years that do overlap

flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
#flow_Alaf_Hills <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Alaf_Hills.csv")
flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
#flow_Jason1 <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Jason1.csv")

full_flow_range <- read.csv("01_Data/full_flow_range.csv")
#full_flow_range <- read.csv("/blue/carpena/caseyharris/Ch2/full_flow_range.csv")
unique(full_flow_range$Q_cfs_log_round)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(!str_detect(wq_names, "Temperature|Organic nitrogen|Chloride"))
unique(wq_names$wq_names)

stopCluster(cl = cluster)
library(foreach)
library(doParallel)
n_cores <- detectCores(logical=FALSE)
n_cores
cluster <- makeCluster(12)
registerDoParallel(cluster)

# library(parallel)
# cl <- makePSOCKcluster(6)
# setDefaultCluster(cl)
# adder <- function(a, b) a + b #???
# clusterExport(NULL, c('adder'))
# clusterEvalQ(NULL, library(tidyverse)) #???
# parLapply(NULL, 1:8, function(z) adder(z, 100)) #???

#how can I be sure that foreach is not trying to access the same jags info while working

i=1
foreach(i = 1:length(wq_names$wq_names),
        .packages=c('tidyverse', 'mgcv', 'jagsUI')) %dopar% {
#for (i in wq_names$wq_names) {
  
  title <- gsub(".csv", "", wq_names$wq_names[i])
  
  wq <- read.csv(paste("01_Data/Orig wq and flow/WQ/", title, ".csv", sep="")) #water quality data
  #wq <- read.csv(paste("/blue/carpena/caseyharris/Ch2/WQ/", title, ".csv", sep="")) #water quality data
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen) %>%
    filter(Date>=as.Date("2010-01-01") & Date<as.Date("2020-01-01")) #only using 2010 onward so change over time won't be as much of an issue
  
  if (str_detect(title, "Alafia")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Alafia") %>%
      select(Date, Q_cfs_log_round)
  } else if (str_detect(title, "Morris")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough") %>%
      select(Date, Q_cfs_log_round)
  }

  wq_flow <- flow0 %>%
    mutate(Date = as.Date(Date)) %>%
    left_join(wq0, by="Date") %>%
    select(Q_cfs_log_round, ConcAve) %>%
    mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round))) %>%
    filter(!is.na(ConcAve))
  
  addt_flow <- data.frame("Q_cfs_log_round" = setdiff(full_flow_range$Q_cfs_log_round, wq_flow$Q_cfs_log_round), "ConcAve"=NA_real_)
  addt_flow <- addt_flow %>%
    mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round)),
           ConcAve = as.numeric(as.character(ConcAve)))
  
  wq_flow1 <- wq_flow %>%
    bind_rows(addt_flow) %>%
    arrange(Q_cfs_log_round, ConcAve) %>%
    mutate(title = title)
  
  ##########################################################
  #JAGS version
  wq_flow2 <- wq_flow1 %>%
    mutate(wq_orig = ConcAve) %>%
    select(title, Q_cfs_log_round, wq_orig)
  
  a=1
  for (a in 1:5) { #number of times to split dataset -- not sure what sort of cross-validation would actually be best
                   #maybe bootstrap aggregating https://en.wikipedia.org/wiki/Bootstrap_aggregating
    
    #Splitting the dataset
    wq_flow2_unobs_only <- wq_flow2 %>%
      filter(is.na(wq_orig)) %>%
      mutate(wq_cal = NA_real_)
    
    wq_flow2_obs_only <- wq_flow2 %>%
      filter(!is.na(wq_orig))
    n_split <- nrow(wq_flow2_obs_only)
    set.seed(a*i)
    split <- sample(c(TRUE, FALSE), n_split, replace=TRUE, prob=c(0.75, 0.25)) #the probability of never using a particular value from my full dataset across the five splits is less than 1/1000
    
    cal <- wq_flow2_obs_only[split, ]
    val <- wq_flow2_obs_only[!split, ]
    
    val <- val %>%
      mutate(wq_cal = NA_real_)
    
    cal_val_df <- cal %>%
      mutate(wq_cal = wq_orig) %>%
      bind_rows(val) %>%
      bind_rows(wq_flow2_unobs_only) %>%
      arrange(Q_cfs_log_round, wq_orig)
    
    tmp_jags_code <- paste("02_Scripts/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
    #tmp_jags_code <- paste("/blue/carpena/caseyharris/Ch2/JAGS/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
    set.seed(a*i)
    tmp_model_obs_des <- jagam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=gaussian, na.action=na.pass) #gives me the design matrix I need
    set.seed(a*i)
    tmp_model_obs <- jagam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=Gamma(link="log")) #give me the jags script I need
    #design matrices for tmp_model_obs and tmp_model_des are identical with na.action=na.omit,
    #but when I use na.pass it differs slightly (why?)
    own_design_matrix <- tmp_model_obs_des$jags.data
    
    #add something like this to JAGS code to estimate y values:
    # for (i in 1:n) {
    #   y_est[i] <- dgamma(r, r/mu[i])
    # }
    
    #Set parameters to monitor
    params = c("mu", "r") #could include y or y_est when working, could include b, eta, and scale
    #b are the GAM parameters, eta is needed for the log link, mu is expected values of mean water quality, 
    #r and scale describe the gamma distribution
    #y is response values of water quality
    #how can I include Q_cfs_log_round in output?
    
    #Run model
    jags_code <- paste("02_Scripts/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="")
    #jags_code <- paste("/blue/carpena/caseyharris/Ch2/JAGS/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="")
    set.seed(a*i)
    mod1 <- jags(model.file = jags_code,
                 parameters.to.save = params,
                 data = own_design_matrix, #includes NAs
                 #inits = tmp_model_obs$jags.ini, #error message when I include inits
                 n.chains = 4, #number of chains
                 n.adapt = NULL,
                 n.burnin = 1000, #number of iterations to discard as burn-in
                 n.iter = 2250, #number of iterations
                 n.thin = 25) #interval to thin
    #gives me 200 posterior samples, x5 with the 5 splits for 1000 posterior samples per Q, is that enough?
    
    #mod1$samples and mod1$sims.list are the same, just arranged differently
    mcmc <- do.call('rbind', mod1$samples)
    
    # test <- own_design_matrix$X %*% t(mcmc[,1:4]) #I think this would be another way of getting eta values,
    # test <- exp(test) #mu values
    # then draw y values
    
    n_flows <- length(cal_val_df$Q_cfs_log_round)
    mcmc_df <- data.frame(mcmc)
    mcmc_df0 <- mcmc_df[,1:n_flows] #mu
    mcmc_df1 <- mcmc_df[,n_flows+1] #r

    mcmc_save_jags <- data.frame("split"=a, "r"=mcmc_df1, mcmc_df0)
    mcmc_save_jags <- as.data.table(mcmc_save_jags)
    
    cal_val_jags <- cal_val_df %>%
      mutate(cal_val = case_when(!is.na(wq_cal) & !is.na(wq_orig) ~ "cal",
                                 is.na(wq_cal) & !is.na(wq_orig) ~ "val",
                                 TRUE ~ "neither"),
             split = a) %>%
      select(title, Q_cfs_log_round, wq_orig, cal_val, split)
    cal_val_jags <- as.data.table(cal_val_jags)
    
    if (a==1) {
      write.table(mcmc_save_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC/", title, ".csv", sep=""), row.names=FALSE)
      #write.table(mcmc_save_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title, ".csv", sep=""), row.names=FALSE)
      write.table(cal_val_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title, ".csv", sep=""), row.names=FALSE)
      #write.table(cal_val_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/Cal_val/", title, ".csv", sep=""), row.names=FALSE)
    } else {
      write.table(mcmc_save_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
      #write.table(mcmc_save_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
      write.table(cal_val_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
      #write.table(cal_val_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/Cal_val/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
    }

    rm(mcmc)
    rm(mcmc_df)
    rm(mcmc_df0)
    rm(mcmc_df1)
    rm(mcmc_save_jags)
    rm(cal_val_jags)
    print(paste("Split", a))
    
  }
  
  print(title)
  
}

stopCluster(cl = cluster)

#write.csv(NSE, "C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/NSE.csv", row.names=FALSE)

  # #Getting 95% credible intervals
  # mcmc_summary <- mcmc_longer %>%
  #   group_by(Q_cfs_log_round) %>%
  #   summarise(mu_mean = mean(mu),
  #             mu_025 = quantile(mu, .025),
  #             mu_975 = quantile(mu, .975),
  #             y_mean = mean(y),
  #             y_025 = quantile(y, .025),
  #             y_975 = quantile(y, .975)) %>%
  #   ungroup() %>%
  #   mutate(title = title) 
  # 
  # cal_val_est <- cal_val_df %>%
  #   mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round))) %>%
  #   mutate(y_est_100 = mcmc_longer[((100*n_flows)+1):(101*n_flows),10]$y) %>% #the 100th simulation, could choose anything
  #   mutate(cal_or_val = case_when(!is.na(wq_cal) ~ "Model calibration",
  #                               !is.na(wq_orig) & is.na(wq_cal) ~ "Model validation",
  #                               TRUE ~ "neither")) 
  # 
  # max_flow_sampled <- max(cal_val_est$Q_cfs_log_round[!is.na(cal_val_est$wq_orig)])
  # min_flow_sampled <- min(cal_val_est$Q_cfs_log_round[!is.na(cal_val_est$wq_orig)])
  # 
  # cal_val_est <- cal_val_est %>%
  #   mutate(method = "JAGS",
  #          max_flow_sampled = max_flow_sampled,
  #          min_flow_sampled = min_flow_sampled)
  #   
  # mcmc_summary1 <- mcmc_summary %>%
  #   mutate(in_or_out = case_when(Q_cfs_log_round<min_flow_sampled | Q_cfs_log_round>max_flow_sampled ~ "OOR",
  #                                Q_cfs_log_round>=min_flow_sampled & Q_cfs_log_round<=max_flow_sampled ~ "in range",
  #                                TRUE ~ NA)) %>%
  #   mutate(method = "JAGS",
  #          max_flow_sampled = max_flow_sampled,
  #          min_flow_sampled = min_flow_sampled)
  # 
  # jags_cal_val <- jags_cal_val %>%
  #   bind_rows(cal_val_est)
  # 
  # jags_summary <- jags_summary %>%
  #   bind_rows(mcmc_summary1)
  # 
  # #NSE=1 perfect model, NSE=0 as good as using the mean, NSE<0 worse than using the mean
  # a = cal_val_est[cal_val_est$cal_or_val=="Model validation",]$wq_orig
  # b = cal_val_est[cal_val_est$cal_or_val=="Model validation",]$y_est_100
  # nse_bayes <- 1 - (sum((a - b)^2) / sum((a - mean(a))^2))
  # nse_bayes_alt <- 1 - (sum(abs(a - b)) / sum(abs(a - mean(a))))
  # #alt version may be less sensitive to outliers
  # #there's probably a better way to calculate this (average NSE?), this uses only one set of simulated values
  # 
  # #Plot shows in range only (labels provide min/max)
  # #mean/error bars based on calibration dataset
  # #original values
  # #estimated values
  # ggplot(filter(mcmc_summary1, in_or_out=="in range")) +
  #   geom_line(aes(exp(Q_cfs_log_round), y_025)) +
  #   geom_line(aes(exp(Q_cfs_log_round), y_975)) +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_mean), linetype="dashed") +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_025)) +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_975)) +
  #   geom_point(data=filter(cal_val_est, cal_or_val!="neither"), aes(exp(Q_cfs_log_round), wq_orig, color="Measured"), shape=1) +
  #   geom_point(data=filter(cal_val_est, cal_or_val!="neither"), aes(exp(Q_cfs_log_round), y_est_100, color="Simulated"), shape=1) +
  #   ggtitle(paste0("JAGS version: ", title, sep="")) +
  #   ylab("Water quality (units)") +
  #   xlab("Flow (ft^3/s)") +
  #   scale_color_manual(values=c("Measured"="green", "Simulated"="black")) +
  #   theme_bw() +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " jags.png", sep=""), width=6, height=3.75)
  # 
  # #XY plot for orig/calib and orig/valid
  # ggplot(filter(cal_val_est, cal_or_val!="neither"), aes(wq_orig, y_est_100, color=cal_or_val)) +
  #   geom_point(shape=1) + 
  #   geom_abline(aes(slope=1, intercept=0), linetype="dashed") +
  #   ggtitle(paste0("JAGS version: ", title, sep=""), subtitle=paste0("NSE = ", round(nse_bayes, 2), " and NSE_alt = ", round(nse_bayes_alt, 2), sep="")) +
  #   ylab("Simulated water quality") +
  #   xlab("Observed water quality") +
  #   scale_color_manual(values=c("Model calibration"="blue", "Model validation"="red")) +
  #   theme_bw() +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " jags nse.png", sep=""), width=6, height=3.75)
  # 
  # ggplot(cal_val_est, aes(exp(Q_cfs_log_round), wq_orig)) +
  #   geom_point(shape=1, aes(color="Measured")) +
  #   geom_point(aes(exp(Q_cfs_log_round), y_est_100, color="Simulated"), shape=1) +
  #   ggtitle(paste0("JAGS version ALL flows: ", title, sep="")) +
  #   ylab("Water quality") +
  #   xlab("Observed water quality") +
  #   scale_color_manual(values=c("Simulated"="black", 
  #                               "Measured"="green")) +
  #   theme_bw() +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " jags ALL.png", sep=""), width=6, height=3.75)
  # 
  # # #modifying the code to use rjags and then 
  # # #using coda() or coda.samples() might help me get estimated y values
  # # #examples on cran NOT WORKING YET for validation dataset
  # # require(rjags)
  # # tmp_model_obs$jags.ini$.RNG.name <- "base::Mersenne-Twister" ## setting RNG
  # # tmp_model_obs$jags.ini$.RNG.seed <- 6 ## how to set RNG seed
  # # mod2 <-jags.model(jags_code, data=tmp_model_obs$jags.data,
  # #                   inits=tmp_model_obs$jags.ini, n.chains=3)
  # # sam <- jags.samples(mod2, c("b", "eta", "mu", "r", "scale"),
  # #                     n.adapt=1000, n.burnin=2000, n.iter=4500, thin=25)
  # # jam <- sim2jam(sam, tmp_model_obs$pregam)
  # # plot(jam) #?
  # # jam
  # # pred_y <- predict(jam, newdata=val) #gives me eta values?
  # # 
  # # mcmc_longer$wq_obs <- rep(cal$wq, 300) #make sure order is not messed up or join more carefully

  # #GAM only version
  # mod_gam <- gam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), family=Gamma(link="log"), data=cal_val_df)
  # 
  # pred_mean <- predict.gam(mod_gam, newdata=cal_val_df)
  # cal_val_df$pred_mean <- c(pred_mean)
  # 
  # cal_val_df_unique <- cal_val_df %>%
  #   select(Q_cfs_log_round) %>%
  #   unique()
  # 
  # sim_wq <- simulate(mod_gam, nsim=300, data=cal_val_df_unique)
  # #simulate() gets the predicted values and then randomly draws from their probability distribution
  # 
  # j=1
  # n_obs_gam <- length(cal_val_df_unique$Q_cfs_log_round)
  # sim_summary <- data.frame()
  # for (j in 1:n_obs_gam) {
  #   dat <- sim_wq[j,]
  #   sim_low <- quantile(dat, .025)
  #   sim_high <- quantile(dat, .975)
  #   sim_mean <- mean(dat)
  #   sim_median <- median(dat)
  #   sim_val <- sim_wq[j,100] #the 100th set of simulated y values, could pick anything
  #   
  #   sim_summary_new <- data.frame(sim_low, sim_high, sim_mean, sim_median, sim_val)
  #   sim_summary <- sim_summary %>%
  #     bind_rows(sim_summary_new)
  #   
  # }
  # 
  # sim_summary1 <- sim_summary %>%
  #   bind_cols(cal_val_df_unique) %>%
  #   mutate(in_or_out = case_when(Q_cfs_log_round<min_flow_sampled | Q_cfs_log_round>max_flow_sampled ~ "OOR",
  #                                Q_cfs_log_round>=min_flow_sampled & Q_cfs_log_round<=max_flow_sampled ~ "in range",
  #                                TRUE ~ NA)) %>%
  #   mutate(method = "GAM",
  #          max_flow_sampled = max_flow_sampled,
  #          min_flow_sampled = min_flow_sampled)
  # 
  # sim_wq_cal_val <- simulate(mod_gam, nsim=300, data=cal_val_df)
  # 
  # cal_val_gam <- cal_val_df %>%
  #   mutate(y_gam_100 = sim_wq_cal_val[,100]) %>%
  #   mutate(cal_or_val = case_when(!is.na(wq_cal) ~ "Model calibration",
  #                                 !is.na(wq_orig) & is.na(wq_cal) ~ "Model validation",
  #                                 TRUE ~ "neither")) %>%
  #   mutate(method = "GAM",
  #        max_flow_sampled = max_flow_sampled,
  #        min_flow_sampled = min_flow_sampled)
  # 
  # gam_cal_val <- gam_cal_val %>%
  #   bind_rows(cal_val_gam)
  # 
  # gam_summary <- gam_summary %>%
  #   bind_rows(sim_summary1)
  # 
  # ggplot(filter(sim_summary1, in_or_out=="in range")) +
  #   geom_line(aes(exp(Q_cfs_log_round), sim_low)) +
  #   geom_line(aes(exp(Q_cfs_log_round), sim_high)) +
  #   geom_line(aes(exp(Q_cfs_log_round), sim_mean), linetype="dashed") +
  #   geom_point(data=filter(cal_val_gam, cal_or_val!="neither"), aes(exp(Q_cfs_log_round), wq_orig, color="Measured"), shape=1) +
  #   geom_point(data=filter(cal_val_gam, cal_or_val!="neither"), aes(exp(Q_cfs_log_round), y_gam_100, color="Simulated"), shape=1) +
  #   ggtitle(paste0("GAM version: ", title, sep="")) +
  #   ylab("Water quality (units)") +
  #   xlab("Flow (ft^3/s)") +
  #   theme_bw() +
  #   scale_color_manual(values=c("Simulated"="black", 
  #                      "Measured"="green")) +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " gam.png", sep=""), width=6, height=3.75)
  # 
  # a = cal_val_gam[cal_val_gam$cal_or_val=="Model validation",]$wq_orig
  # b = cal_val_gam[cal_val_gam$cal_or_val=="Model validation",]$y_gam_100
  # nse_gam <- 1 - (sum((a - b)^2) / sum((a - mean(a))^2))
  # nse_gam_alt <- 1 - (sum(abs(a - b)) / sum(abs(a - mean(a))))
  # 
  # ggplot(filter(cal_val_gam, cal_or_val!="neither"), aes(wq_orig, y_gam_100, color=cal_or_val)) +
  #   geom_point(shape=1) + 
  #   geom_abline(slope=1, intercept=0, linetype="dashed") +
  #   ggtitle(paste0("GAM version: ", title, sep=""), subtitle=paste0("NSE = ", round(nse_gam, 2), " and NSE_alt = ", round(nse_gam_alt, 2), sep="")) +
  #   ylab("Simulated water quality") +
  #   xlab("Observed water quality") +
  #   scale_color_manual(values=c("Model calibration"="blue", "Model validation"="red")) +
  #   theme_bw() +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " gam nse.png", sep=""), width=6, height=3.75)
  # 
  # ggplot(cal_val_gam, aes(exp(Q_cfs_log_round), wq_orig)) +
  #   geom_point(shape=1, aes(color="Measured")) +
  #   geom_point(aes(exp(Q_cfs_log_round), y_gam_100, color="Simulated"), shape=1) +
  #   ggtitle(paste0("GAM version ALL flows: ", title, sep="")) +
  #   ylab("Water quality") +
  #   xlab("Observed water quality") +
  #   scale_color_manual(values=c("Simulated"="black", 
  #                               "Measured"="green")) +
  #   theme_bw() +
  #   theme(legend.title=element_blank(), legend.position="right", legend.text=element_text(size=10), plot.title=element_text(size=10), plot.subtitle=element_text(size=10), axis.title=element_text(size=10), axis.text=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Initial C-D relationships/", title, " gam ALL.png", sep=""), width=6, height=3.75)
  # 
  # write.csv(jags_cal_val, "JAGS cal val.csv", row.names=FALSE)
  # write.csv(gam_cal_val, "GAM cal val.csv", row.names=FALSE)
  # write.csv(jags_summary, "JAGS summary.csv", row.names=FALSE)
  # write.csv(gam_summary, "GAM summary.csv", row.names=FALSE)
  

