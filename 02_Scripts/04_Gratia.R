library(mgcv)
library(gratia)
library(tidyverse)
#library(jagsUI)
#library(data.table)

#use retrospective or the climate model we're talking about to compare time periods
#nldas would tell us how good the model is
#ch1 told us about imp. of land management
#ch2 holds land management const, look at just streamflow
#could compare years that do overlap

flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
#flow_Alaf_Hills <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Alaf_Hills.csv")
#flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
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

# stopCluster(cl = cluster)
# library(foreach)
# library(doParallel)
# n_cores <- detectCores(logical=FALSE)
# n_cores
# cluster <- makeCluster(12)
# registerDoParallel(cluster)

# library(parallel)
# cl <- makePSOCKcluster(6)
# setDefaultCluster(cl)
# adder <- function(a, b) a + b #???
# clusterExport(NULL, c('adder'))
# clusterEvalQ(NULL, library(tidyverse)) #???
# parLapply(NULL, 1:8, function(z) adder(z, 100)) #???

# how can I be sure that foreach is not trying to access the same jags info while working

i=1
# foreach(i = 1:length(wq_names$wq_names),
#         .packages=c('tidyverse', 'mgcv', 'gratia')) %dopar% {
for (i in 1:length(wq_names$wq_names)) {
  
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
  #Gratia version
  wq_flow2 <- wq_flow1 %>%
    mutate(wq_orig = ConcAve) %>%
    select(title, Q_cfs_log_round, wq_orig)
  
  #Should I do 1000 splits and simulate one set of values from each (would give me cal/val, but estimates seem to have too large of spread),
  #Or should I use all data in one go and simulate 1000 sets of values from that fit (would not give me cal/val, but spread of estimates is similar to observed data...actually not sure about that)?
  
  #sim_wq_all <- data.frame()
  #pred_wq_all <- data.frame()
  #a=1
  #for (a in 1:1000) { #number of times to split dataset -- not sure what sort of cross-validation would actually be best
                      #maybe bootstrap aggregating https://en.wikipedia.org/wiki/Bootstrap_aggregating
    
    # #Splitting the dataset
    # wq_flow2_unobs_only <- wq_flow2 %>%
    #   filter(is.na(wq_orig)) %>%
    #   mutate(wq_cal = NA_real_)
    # 
    # wq_flow2_obs_only <- wq_flow2 %>%
    #   filter(!is.na(wq_orig))
    # n_split <- nrow(wq_flow2_obs_only)
    # set.seed(a*i)
    # split <- sample(c(TRUE, FALSE), n_split, replace=TRUE, prob=c(0.75, 0.25))
    # 
    # cal <- wq_flow2_obs_only[split, ]
    # val <- wq_flow2_obs_only[!split, ]
    # 
    # val <- val %>%
    #   mutate(wq_cal = NA_real_)
    # 
    # cal_val_df <- cal %>%
    #   mutate(wq_cal = wq_orig) %>%
    #   bind_rows(val) %>%
    #   bind_rows(wq_flow2_unobs_only) %>%
    #   arrange(Q_cfs_log_round, wq_orig)
    
    cal_val_df <- wq_flow2 #ONLY IF NOT USING SPLITS
    mod0 <- gam(wq_orig ~ s(Q_cfs_log_round, bs="tp", k=4), family=Gamma(link="log"), data=cal_val_df)
    #mod0 <- gam(wq_cal ~ s(Q_cfs_log_round, bs="tp", k=4), family=Gamma(link="log"), data=cal_val_df)
    
    sim_wq <- simulate(mod0, nsim=1000, data=cal_val_df) #simulate() gets the predicted values and then randomly draws from their probability distribution
    sim_wq <- data.frame(sim_wq)
    sim_wq1 <- cal_val_df %>%
      # mutate(split = a) %>%
      bind_cols(sim_wq)
    # sim_wq_all <- sim_wq_all %>%
    #   bind_rows(sim_wq1)
    
    pred_mean <- predict.gam(mod0, newdata=cal_val_df)
    cal_val_df1 <- cal_val_df
    # cal_val_df1$split <- a
    cal_val_df1$pred_mean <- c(exp(pred_mean))
    # pred_wq_all <- pred_wq_all %>%
    #   bind_rows(cal_val_df1)
  
    # print(a)  
  # }
  
  write.csv(sim_wq1, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/WQ per flow sim/", title, ".csv", sep=""), row.names=FALSE)
  write.csv(cal_val_df1, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/WQ per flow pred/", title, ".csv", sep=""), row.names=FALSE)
    
  # write.csv(sim_wq_all, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/WQ per flow sim/", title, ".csv", sep=""), row.names=FALSE)
  # write.csv(pred_wq_all, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/WQ per flow pred/", title, ".csv", sep=""), row.names=FALSE)
  
  #how to plot?
  sim_wq1_long <- sim_wq1 %>%
    pivot_longer(cols=starts_with("X"), names_to="sim", values_to="est") %>%
    #filter(str_detect(sim, "90")) %>%
    group_by(Q_cfs_log_round) %>%
    summarise(low = quantile(est, .05),
              high = quantile(est, .95))
  p <- ggplot(cal_val_df1, aes(Q_cfs_log_round, pred_mean)) +
    #geom_point(data=sim_wq1_long, aes(Q_cfs_log_round, est), color="gray", shape=1) +
    geom_line(data=sim_wq1_long, aes(Q_cfs_log_round, low)) +
    geom_line(data=sim_wq1_long, aes(Q_cfs_log_round, high)) +
    geom_line() +
    geom_point(aes(Q_cfs_log_round, wq_orig))
  ggsave(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Gratia CQ/", title, ".png", sep=""), plot=p, width=8, height=8)

}

#now join to actual days and calculate summaries
 
  # #NSE=1 perfect model, NSE=0 as good as using the mean, NSE<0 worse than using the mean
  # a = cal_val_est[cal_val_est$cal_or_val=="Model validation",]$wq_orig
  # b = cal_val_est[cal_val_est$cal_or_val=="Model validation",]$y_est_100
  # nse_bayes <- 1 - (sum((a - b)^2) / sum((a - mean(a))^2))
  # nse_bayes_alt <- 1 - (sum(abs(a - b)) / sum(abs(a - mean(a))))
  # #alt version may be less sensitive to outliers
  # #there's probably a better way to calculate this (average NSE?), this uses only one set of simulated values
