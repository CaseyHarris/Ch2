#This script uses MCMC to estimate expected values of water quality at 
#given streamflow values, as well as parameters for sampling uncertainty
#of water quality.
#Together these define distributions of water quality at given streamflow
#values, used in later scripts.

library(mgcv)
library(tidyverse)
library(jagsUI)
library(data.table)

flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
#flow_Alaf_Hills <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Alaf_Hills.csv")
flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
#flow_Jason1 <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Jason1.csv")

full_flow_range <- read.csv("01_Data/full_flow_range.csv")
#full_flow_range <- read.csv("/blue/carpena/caseyharris/Ch2/full_flow_range.csv")
unique(full_flow_range$Q_cfs_log_round)

wq_names <- list.files("01_Data/Redone wq and flow")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(str_detect(wq_names, "Alkalinity|Color|Fluoride|Iron|Manganese|Nitrogen|TOC|Phosphorus|Turbidity"))
unique(wq_names$wq_names)

#stopCluster(cl = cluster)
library(foreach)
library(doParallel)
n_cores <- detectCores(logical=FALSE)
n_cores
cluster <- makeCluster(3)
registerDoParallel(cluster)

# library(parallel)
# cl <- makePSOCKcluster(6)
# setDefaultCluster(cl)
# adder <- function(a, b) a + b #???
# clusterExport(NULL, c('adder'))
# clusterEvalQ(NULL, library(tidyverse)) #???
# parLapply(NULL, 1:8, function(z) adder(z, 100)) #???

#make sure that foreach is not trying to access the same jags info while working

i=1
foreach(i = 1:length(wq_names$wq_names),
        .packages=c('tidyverse', 'mgcv', 'jagsUI', 'data.table')) %dopar% {
#for (i in wq_names$wq_names) {
  
  title <- gsub(".csv", "", wq_names$wq_names[i])
  
  wq <- read.csv(paste("01_Data/Redone wq and flow/", title, ".csv", sep="")) #water quality data
  #wq <- read.csv(paste("/blue/carpena/caseyharris/Ch2/WQ/", title, ".csv", sep="")) #water quality data
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen) %>%
    filter(Date>=as.Date("2013-01-01") & Date<as.Date("2023-01-01")) #only recent 10 years so change over time won't be as much of an issue
  
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
    select(title, Q_cfs_log_round, wq_orig) %>%
    arrange(Q_cfs_log_round)
  
  # Below is how I would have split the dataset (not doing that now)
  # a=1
  # for (a in 1:10) { #number of times to split dataset -- not sure what sort of cross-validation would actually be best
  #                  #maybe bootstrap aggregating https://en.wikipedia.org/wiki/Bootstrap_aggregating
  #   
  #   #Splitting the dataset
  #   wq_flow2_unobs_only <- wq_flow2 %>%
  #     filter(is.na(wq_orig)) %>%
  #     mutate(wq_cal = NA_real_)
  #   
  #   wq_flow2_obs_only <- wq_flow2 %>%
  #     filter(!is.na(wq_orig))
  #   n_split <- nrow(wq_flow2_obs_only)
  #   set.seed(a*i)
  #   split <- sample(c(TRUE, FALSE), n_split, replace=TRUE, prob=c(0.8, 0.2))
  #   
  #   cal <- wq_flow2_obs_only[split, ]
  #   val <- wq_flow2_obs_only[!split, ]
  #   
  #   val <- val %>%
  #     mutate(wq_cal = NA_real_)
  #   
  #   cal_val_df <- cal %>%
  #     mutate(wq_cal = wq_orig) %>%
  #     bind_rows(val) %>%
  #     bind_rows(wq_flow2_unobs_only) %>%
  #     arrange(Q_cfs_log_round)
    
    # tmp_jags_code <- paste("02_Scripts/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
    # #tmp_jags_code <- paste("/blue/carpena/caseyharris/Ch2/JAGS/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
    # set.seed(a*i)
    # tmp_model_obs_des <- jagam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=gaussian, na.action=na.pass) #gives me the design matrix I need
    # set.seed(a*i)
    # tmp_model_obs <- jagam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), data=cal_val_df, file=tmp_jags_code, family=Gamma(link="log")) #give me the jags script I need
    # #design matrices for tmp_model_obs and tmp_model_des are identical with na.action=na.omit,
    # #but when I use na.pass it differs slightly (why?)
    # own_design_matrix <- tmp_model_obs_des$jags.data
  
  tmp_jags_code <- paste("02_Scripts/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
  #tmp_jags_code <- paste("/blue/carpena/caseyharris/Ch2/JAGS/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="") #where the jags code file will be written
  set.seed(i)
  tmp_model_obs_des <- jagam(wq_orig ~ s(Q_cfs_log_round, bs="tp", k=4), data=wq_flow2, file=tmp_jags_code, family=gaussian, na.action=na.pass) #gives me the design matrix I need
  set.seed(i)
  tmp_model_obs <- jagam(wq_orig ~ s(Q_cfs_log_round, bs="tp", k=4), data=wq_flow2, file=tmp_jags_code, family=Gamma(link="log")) #give me the jags script I need
  #design matrices for tmp_model_obs and tmp_model_des are identical with na.action=na.omit,
  #but when I use na.pass it differs slightly (why?)
  own_design_matrix <- tmp_model_obs_des$jags.data
    
    #add something like this to JAGS code to estimate y values:
    # for (i in 1:n) {
    #   y_est[i] <- dgamma(r, r/mu[i])
    # } #this never worked for me, I ended up doing it in a separate script, which runs very slowly
    
    #Set parameters to monitor
    params = c("mu", "r") #could include y or y_est when working, could include b, eta, and scale
    #b are the GAM parameters, eta is needed for the log link, mu is expected values of mean water quality, 
    #r and scale describe the gamma distribution
    #y is response values of water quality
    #how can I include Q_cfs_log_round in output?
    
    #Run model
    jags_code <- paste("02_Scripts/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="")
    #jags_code <- paste("/blue/carpena/caseyharris/Ch2/JAGS/tmp_jags_code 2024-08-02 unedited ", title, ".R", sep="")
    set.seed(i)
    mod1 <- jags(model.file = jags_code,
                 parameters.to.save = params,
                 data = own_design_matrix, #includes NAs
                 #inits = tmp_model_obs$jags.ini, #error message when I include inits
                 n.chains = 4, #number of chains
                 n.adapt = NULL,
                 n.burnin = 1000, #number of iterations to discard as burn-in
                 n.iter = 1500, #number of iterations
                 n.thin = 20) #interval to thin
    #gives me 100 posterior samples, is that enough?
    
    #mod1$samples and mod1$sims.list are the same, just arranged differently
    mcmc <- do.call('rbind', mod1$samples)
    
    # test <- own_design_matrix$X %*% t(mcmc[,1:4]) #I think this would be another way of getting eta values,
    # test <- exp(test) #mu values
    # then draw y values
    
    n_flows <- length(wq_flow2$Q_cfs_log_round)
    mcmc_df <- data.frame(mcmc)
    mcmc_df0 <- mcmc_df[,1:n_flows] #mu
    mcmc_df1 <- mcmc_df[,n_flows+1] #r

    mcmc_save_jags <- data.frame("r"=mcmc_df1, mcmc_df0)
    mcmc_save_jags <- as.data.table(mcmc_save_jags)
    
    # cal_val_jags <- cal_val_df %>%
    #   mutate(cal_val = case_when(!is.na(wq_cal) & !is.na(wq_orig) ~ "cal",
    #                              is.na(wq_cal) & !is.na(wq_orig) ~ "val",
    #                              TRUE ~ "neither"),
    #          split = a) %>%
    #   select(title, Q_cfs_log_round, wq_orig, cal_val, split)
    # cal_val_jags <- as.data.table(cal_val_jags)
    
    write.csv(mcmc_save_jags, paste("03_Results/MCMC/", title, ".csv", sep=""), row.names=FALSE)
    write.csv(wq_flow2, paste("01_Data/CQ/", title, ".csv", sep=""), row.names=FALSE)
    
    # if (a==1) {
    #   write.table(mcmc_save_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC/", title, ".csv", sep=""), row.names=FALSE)
    #   #write.table(mcmc_save_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title, ".csv", sep=""), row.names=FALSE)
    #   write.table(cal_val_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title, ".csv", sep=""), row.names=FALSE)
    #   #write.table(cal_val_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/Cal_val/", title, ".csv", sep=""), row.names=FALSE)
    # } else {
    #   write.table(mcmc_save_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
    #   #write.table(mcmc_save_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
    #   write.table(cal_val_jags, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
    #   #write.table(cal_val_jags, paste("/blue/carpena/caseyharris/Ch2/JAGS/Cal_val/", title, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
    # }

    rm(mcmc)
    rm(mcmc_df)
    rm(mcmc_df0)
    rm(mcmc_df1)
    rm(mcmc_save_jags)
    #rm(cal_val_jags)
  
  print(title)
  
}

stopCluster(cl = cluster)



