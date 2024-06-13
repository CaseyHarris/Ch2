library(tidyverse)
library(lubridate)
library(mgcv)
library(jagsUI)

#How does water quality change with streamflow, or streamflow and temperature?
#What is the probability of water quality exceeding relevant thresholds?
#Report future distributions of water quality and uncertainty

#Using a generalized additive model for flexible relationships
#Using Bayesian analysis to provide credible intervals (interval that contains X% of values)
#which are more easily interpretable than frequentist confidence intervals for making decisions
#also useful for hierarchical models, constraining outcomes, examining the posterior in various ways

#Look at actual data
data0 <- read.csv("Data/actual data.csv")
data0 <- data0 %>%
  mutate(date = as.Date(date),
         row = row_number(),
         log_flow = log10(flow),
         yday = yday(date)) %>%
  select(date, yday, flow, log_flow, wq, log_wq)

ggplot(data0, aes(wq)) +
  geom_histogram() # looks lognormal

ggplot(data0, aes(flow)) +
  geom_histogram(bins=100) # also looks lognormal
#I need to log-transform TOC and streamflow, even when using a GAM? maybe not

#Test out GAM
mod0 <- gam(log_wq ~ s(log_flow, bs="tp", k=4), data=data0)
mod1 <- gam(wq ~ s(flow, bs="tp", k=4), family="scat", data=data0)
mod2 <- gam(wq ~ s(log_flow, bs="tp", k=4), family="scat", data=data0)
mod3 <- gam(wq ~ s(log_flow, bs="tp", k=4), family=scat(link="identity"), data=data0)
mod4 <- gam(wq ~ s(flow, bs="tp", k=4), family=scat(link="log"), data=data0)
mod5 <- gam(wq ~ s(log_flow, bs="tp", k=4), family=scat(link="inverse"), data=data0)

pred_wq0 <- predict.gam(mod0, newdata=data0)
pred_wq1 <- predict.gam(mod1, newdata=data0)
pred_wq2 <- predict.gam(mod2, newdata=data0)
pred_wq3 <- predict.gam(mod3, newdata=data0)
pred_wq4 <- predict.gam(mod4, newdata=data0)
pred_wq5 <- predict.gam(mod5, newdata=data0)

data0$wq_pred0 <- c(10^pred_wq0)
data0$wq_pred1 <- c(pred_wq1)
data0$wq_pred2 <- c(pred_wq2)
data0$wq_pred3 <- c(pred_wq3)
data0$wq_pred4 <- c(pred_wq4)
data0$wq_pred5 <- c(1/pred_wq5)

# sim_wq <- simulate(mod0, nsim=1, data=data0) 
# #simulate() gets the predicted values and then randomly draws from their probability distribution, could read docs to find out how
# data0$wq_pred <- c(pred_wq)
# data0$wq_sim <- sim_wq[,1]

data0_long <- data0 %>%
  pivot_longer(cols=c(wq_pred0, wq_pred1, wq_pred2, wq_pred3, wq_pred4, wq_pred5))

ggplot(data0_long, aes(flow, wq)) +
  geom_point(shape=1, size=2) +
  geom_point(data=data0_long, aes(flow, value), color="red") +
  facet_wrap(~name)
#Want to use scat with link "identity", not sure if flow should be log-transformed or not
#don't know why mod2 and mod3 would give different results if identity is the default

################################################################################
#This section is just organizing the flow data

#Additional flow data (past) and future flow
all_flow <- read.csv("Data/USGS_flow/pH Hillsborough R near Zephyrhills.csv")
# jason_flow <- read.csv("Data/Jason_flow/Chang data business as usual.csv")
# jason_flow <- jason_flow %>%
#   mutate(date = as.Date(date)) #future1 is for some reason missing the very last day
# NOTE TO SELF: update to use this flow data later

CM3_1 <- read.csv("Data/Old/Hills_CM3_future1.csv")
CM3_2 <- read.csv("Data/Old/Hills_CM3_future2.csv")
ESM2G_1 <- read.csv("Data/Old/Hills_ESM2G_future1.csv")
ESM2G_2 <- read.csv("Data/Old/Hills_ESM2G_future2.csv")

flow_por <- all_flow %>%
  mutate(date = as.Date(Date),
         flow = Q,
         log_flow = log10(Q)) %>%
  filter(date>=min(data0$date) & date<=max(data0$date)) %>%
  select(date, flow, log_flow) %>%
  left_join(data0) %>% #change to data0 if using full dataset
  mutate(row = row_number(),
         day_of_year = yday(date),
         per="past")

CM3_1a <- CM3_1 %>%
  mutate(date = seq.Date(from=as.Date("2030-01-01"), to=as.Date("2060-12-29"), by="day")) %>%
  mutate(flow = X162.36/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="CM3_1") %>%
  select(-X162.36)

CM3_2a <- CM3_2 %>%
  mutate(date = seq.Date(from=as.Date("2070-01-01"), to=as.Date("2100-12-30"), by="day")) %>%
  mutate(flow = X162.79/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="CM3_2") %>%
  select(-X162.79)

ESM2G_1a <- ESM2G_1 %>%
  mutate(date = seq.Date(from=as.Date("2030-01-01"), to=as.Date("2060-12-29"), by="day")) %>%
  mutate(flow = X163.97/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="ESM2G_1") %>%
  select(-X163.97)

minESM2G_2a <- min(filter(ESM2G_2, X162.28>0))
ESM2G_2a <- ESM2G_2 %>%
  mutate(X162.28 = case_when(X162.28==0 ~ minESM2G_2a,
                             TRUE ~ X162.28)) %>%
  mutate(date = seq.Date(from=as.Date("2070-01-01"), to=as.Date("2100-12-30"), by="day")) %>%
  mutate(flow = X162.28/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="ESM2G_2") %>%
  select(-X162.28)

data_by_day <- flow_por %>%
  bind_rows(CM3_1a) %>%
  bind_rows(CM3_2a) %>%
  bind_rows(ESM2G_1a) %>%
  bind_rows(ESM2G_2a) #%>% 
#mutate(log_flow = round(log_flow, digits=1)) #include if using dataframe with empty values

################################################################################

#Now for Bayesian version
#I don't know how to get estimates for new values of streamflow and day of year
#I would need to multiply the design matrix by the estimated parameters
#tmp_model$jags.data$X %*% t(mcmc[,1:6]) #calculated mu
#...but how to make the design matrix? (other than letting jagam do it, I don't know)
#So, letting jagam make the design matrix

data_by_day_obs <- data_by_day %>%
  filter(!is.na(wq))

tmp_jags_code <- "Scripts/tmp_jags_code 2024-02-23.R" #where the jags code file will be written
tmp_model_obs <- jagam(wq ~ s(log_flow, bs="tp", k=4), data=data_by_day_obs, file=tmp_jags_code, family=gaussian(link="log"))
tmp_model_unobs <- jagam(wq ~ s(log_flow, bs="tp", k=4), data=data_by_day, file=tmp_jags_code, na.action=na.pass)
#due to probably a bug, I can't run the "unobs" version with family=gaussian(link="log") and na.action=na.pass

#have to change jags file to scat
#haven't made any other changes to the default jagam suggestions,
#except to change NA in unobs to 1/1000 (what jagam recommended when I didn't have missing values of wq)

#Set parameters to monitor
#params = c("b", "tau", "mu", "y") #b are the parameters, tau is precision, mu is expected values, y is estimated values
#params = c("b", "tau", "df", "mu", "y") #b are the parameters, tau is precision, mu is expected values, y is estimated values
params = c("b", "tau", "k", "mu") #b are the parameters, tau is precision, mu is expected values, y is estimated values
params = c("b", "tau", "k", "mu", "y") #b are the parameters, tau is precision, mu is expected values, y is estimated values
#params = c("b", "tau", "df", "mu") #b are the parameters, tau is precision, mu is expected values, y is estimated values
#params = c("b", "U", "V", "df", "tau", "mu") #b are the parameters, tau is precision, mu is expected values, y is estimated values

#Run model
jags_code <- "Scripts/tmp_jags_code_scat3.R" #same for unobs
mod_obs <- jags(model.file = jags_code,
                parameters.to.save = params,
                data = tmp_model_obs$jags.data, #makes the design matrix for me?
                #inits = tmp_model_obs$jags.ini,
                n.chains = 3, #number of chains
                n.adapt = 1000,
                n.burnin = 2000, #number of iterations to discard as burn-in
                n.iter = 4500, #number of iterations
                n.thin = 25, #interval to thin
                DIC = TRUE)

jags_code <- "Scripts/tmp_jags_code_scat4.R" #same for unobs
mod_unobs <- jags(model.file = jags_code,
                  parameters.to.save = params,
                  data = tmp_model_unobs$jags.data, #makes the design matrix for me?
                  #inits = tmp_model_unobs$jags.ini,
                  n.chains = 3, #number of chains
                  n.adapt = 1000,
                  n.burnin = 2000, #number of iterations to discard as burn-in
                  n.iter = 4500, #number of iterations
                  n.thin = 25, #interval to thin
                  DIC = TRUE)

#mod$samples and mod$sims.list are the same, just arranged differently
mcmc_obs <- do.call('rbind', mod_obs$samples)
mcmc_unobs <- do.call('rbind', mod_unobs$samples)

#mu ends up the same as using tmp_model$jags.data$X %*% t(mcmc[,1:6])
#don't know why all the y's are the same
mcmc_df_obs <- data.frame(mcmc_obs)
mcmc_obs_longer <- mcmc_df_obs %>%
  pivot_longer(cols=7:138) %>%
  mutate(flow = rep(data0$flow, 300))

ggplot(data_by_day_obs, aes(flow, wq)) +
  geom_point(data=mcmc_obs_longer, aes(flow, value)) +
  geom_point(color='red') +
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal()
#looks ok!
rm(mcmc_obs_longer)

mcmc_df_unobs <- data.frame(mcmc_unobs)
mcmc_unobs_longer <- mcmc_df_unobs %>%
  pivot_longer(cols=49637:99266) %>%
  mutate(flow = rep(data_by_day$flow, 300))
#error is that it cannot allocate vector of "size 113.6 Mb"

ggplot(data_by_day, aes(flow, wq)) +
  geom_point(data=mcmc_unobs_longer, aes(flow, value)) +
  geom_point(color='red') +
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal()
#looks ok!
rm(mcmc_unobs_longer)

##try to estimate y's for obs
rm(mcmc)
mcmc <- mcmc_obs
res_df <- data.frame()
n_i <- length(mcmc_df_obs$tau)
n_j <- length(data0$wq)
tau_col <- which(colnames(mcmc_df_obs)=="tau")
#df_col <- which(colnames(mcmc_df_obs)=="df")
k_col <- which(colnames(mcmc_df_obs)=="k")
i=10
j=10
for (i in 1:n_i) { #n_i) {
  
  tau_i <- mcmc[i, tau_col]
  #df_i <- mcmc[i, df_col]
  k_i <- mcmc[i, k_col]
  
  for (j in 1:n_j) {
    
    mu_i_j <- mcmc[i, j+k_col]
    #mu_i_j <- mcmc[i, j+df_col]
    
    x <- rnorm(1, mu_i_j, tau_i)
    s <- rgamma(1, k_i/2, k_i/2)
    y_i_j <- x/sqrt(s)
    
    res_df_new <- data.frame(b1=mcmc[i,1], b2=mcmc[i,2], b3=mcmc[i,3], b4=mcmc[i,4], 
                             tau=tau_i, k=k_i,
                             flow_orig=data0$flow[j],
                             yday_orig=data0$yday[j],
                             #var=var,
                             mu=mu_i_j,
                             y=y_i_j,
                             wq_orig=data0$wq[j])
    
    res_df <- res_df %>%
      bind_rows(res_df_new)
    
  }
}

##try to estimate y's for unobs
rm(mcmc)
mcmc <- mcmc_unobs
res_df <- data.frame()
n_i <- length(mcmc_df_unobs$tau)
n_j <- length(data0$wq)
tau_col <- which(colnames(mcmc_df_obs)=="tau")
#df_col <- which(colnames(mcmc_df_obs)=="df")
k_col <- which(colnames(mcmc_df_obs)=="k")
i=10
j=10
for (i in 1:n_i) { #n_i) {
  
  tau_i <- mcmc[i, tau_col]
  #df_i <- mcmc[i, df_col]
  k_i <- mcmc[i, k_col]
  
  for (j in 1:n_j) {
    
    mu_i_j <- mcmc[i, j+k_col]
    #mu_i_j <- mcmc[i, j+df_col]
    
    x <- rnorm(1, mu_i_j, tau_i)
    s <- rgamma(1, k_i/2, k_i/2)
    y_i_j <- x/sqrt(s)
    
    res_df_new <- data.frame(b1=mcmc[i,1], b2=mcmc[i,2], b3=mcmc[i,3], b4=mcmc[i,4], 
                             tau=tau_i, k=k_i,
                             flow_orig=data0$flow[j],
                             yday_orig=data0$yday[j],
                             #var=var,
                             mu=mu_i_j,
                             y=y_i_j,
                             wq_orig=data0$wq[j])
    
    res_df <- res_df %>%
      bind_rows(res_df_new)
    
  }
}

res_df1 <- res_df %>% #discards values less than 0, not correct
  group_by(flow_orig, yday_orig, wq_orig) %>%
  summarise(y_mean = 10^mean(log10(y)),
            y_05 = 10^quantile(log10(y), .05, na.rm=TRUE),
            y_95 = 10^quantile(log10(y), .95, na.rm=TRUE),
            n = n())

res_df2 <- res_df1 %>%
  filter(y_mean<500 & y_95<300)

ggplot(res_df2, aes(flow_orig, y_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin=y_05, ymax=y_95)) +
  geom_point(data=data0, aes(x=flow, y=wq), color="red") +
  geom_point(data=res_df, aes(flow_orig, mu), color="blue") +
  scale_y_log10()

#Should I really be using the log link?
#Means end up a little high, error bars extending below zero can't be calculated
#100+ is unreasonably high for 95th percentile of TOC

b_plot <- as.data.frame(t(mcmc[,1:6]))
b_plot1 <- b_plot %>%
  mutate(b = rownames(b_plot)) %>%
  pivot_longer(cols=c(V1:V240),
               names_to="iteration",
               values_to="param_est") %>%
  mutate(iter_num = rep(seq(from=1, to=240, by=1), 6))

#Plot parameters
ggplot(b_plot1, aes(iter_num, param_est)) +
  geom_line() +
  xlab("Thinned model iterations") +
  ylab("Parameter estimates") +
  scale_x_continuous(limits=c(1, 240), breaks=c(1, 80, 160, 240)) +
  facet_wrap(.~b, scales="free_y") +
  theme_minimal() +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22),
        strip.text=element_text(size=22))







#matching estimated vals (y) with calibration dataset
mcmc_yt <- data.frame(t(mcmc[,17942:35875]))
#mcmc_yt <- data.frame(t(mcmc[,96:183]))

res_y <- data2 %>%
  mutate(log_wq_orig = log_wq) %>%
  select(-log_wq) %>%
  bind_cols(mcmc_yt)

res_y <- data_try %>%
  mutate(log_wq_orig = log_wq) %>%
  select(-log_wq) %>%
  bind_cols(mcmc_yt)
#why are the estimated values (X1:X240) exactly the same as the input dataset values (log_wq_orig)?

#matching estimated means (mu) with calibration dataset
mcmc_mut <- data.frame(t(mcmc[,8:17941]))
#mcmc_mut <- data.frame(t(mcmc[,8:95]))

res_mu <- data2 %>%
  mutate(log_wq_orig = log_wq) %>%
  select(-log_wq) %>%
  bind_cols(mcmc_mut)

# res_mu <- data_try %>%
#   mutate(log_wq_orig = log_wq) %>%
#   select(-log_wq) %>%
#   bind_cols(mcmc_mut)

ggplot(res_y, aes(10^log_flow, 10^log_wq_orig)) +
  geom_point(data=res_y, aes(10^log_flow, 10^X1), shape=1, color="green") +
  #geom_point(data=res_mu, aes(10^log_flow, 10^X1), color="red") +
  geom_point(shape=1) +
  scale_x_log10()
#much bigger range than my input dataset...

#Displaying results/exceedances
#Predict exceedances (TOC above 8)
#for each value of actual predicted flow, what is the distribution of wq
#the prob of being above 8...

#bind to data by day...
data_by_day1 <- data_by_day %>%
  left_join(res_y, by=c("day_of_year", "log_flow"))

#get some kind of mean and range or distribution
i="CM3_1"
dist_df <- data.frame()
for (i in unique(data_by_day1$per)) {
  
  sub <- data_by_day1 %>%
    filter(per==i)
  
  sub_list <- data.frame(per=i, vals=unlist(list(sub[,14:253])))
  sub_list$cume_dist <- cume_dist(sub_list$vals)
  sub_list$per_exceed <- 1 - sub_list$cume_dist
  sub_list$per_rank <- percent_rank(sub_list$vals)
  sub_list$quantilemin <- quantile(sub_list$vals, .01)
  sub_list$quantile50 <- quantile(sub_list$vals, .5)
  sub_list$quantilemax <- quantile(sub_list$vals, .995)
  
  dist_df <- dist_df %>%
    bind_rows(sub_list)
  
}

dist_df <- dist_df %>%
  mutate(time = case_when(per=="past" ~ "2011-2022",
                          per=="CM3_1" | per=="ESM2G_1" ~ "2030-2060",
                          per=="CM3_2" | per=="ESM2G_2" ~ "2070-2100",
                          TRUE ~ per)) %>%
  mutate(mod = case_when(per=="past" ~ "past",
                         per=="CM3_1" | per=="CM3_2" ~ "CM3",
                         per=="ESM2G_1" | per=="ESM2G_2" ~ "ESM2G",
                         TRUE ~ per))

threshold <- dist_df %>%
  filter(vals>log10(7.95) & vals<log10(8.05)) %>% #because filtering by vals==log10(8) wasn't working, rounding errors
  group_by(per, mod, time) %>%
  summarise(thresh=mean(per_exceed))

ggplot(dist_df, aes(per_exceed*100, 10^vals, group=mod, color=mod)) +
  geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
  geom_line(size=2) +
  geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
  scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
  scale_y_continuous(limits=c(.1, 60)) + #specific to analyte
  scale_color_manual(values=c("past"="black", "CM3"="#2C621B", "ESM2G"="#871E03")) +
  ylab("Total organic carbon (mg/l)") +
  xlab("Percent exceedance (%)") +
  facet_wrap(~time, nrow=1) +
  theme_minimal() +
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=22),
        strip.text=element_text(size=22),
        legend.position="none")
#this plot takes a minute or two

#Get credible intervals of exceedances
#A better way to do this would be to change the GAM
#So that the GAM response is the percent exceedance, rather than just the wq value
#(but unsure how to define it correctly)
ci_thresh <- data.frame()
i="CM3_1"
j=10
for (i in unique(data_by_day1$per)) {
  for (j in 10:249) {
    
    sub <- data_by_day1 %>%
      filter(per==i) %>%
      select(all_of(j))
    
    colnames(sub) <- "vals"
    
    abv_8 <- sub %>%
      filter(vals>log10(8))
    
    abv_8 <- length(abv_8$vals)/length(sub$vals)
    new_df <- data.frame(per=i, iter=j, prop=abv_8)
    
    ci_thresh <- ci_thresh %>%
      bind_rows(new_df)
  }
} #this loop takes a couple minutes

#This table is the credible interval results
#For percent exceedance over 8 mg/l TOC, and uncertainty around that
ci_thresh_summary <- ci_thresh %>%
  group_by(per) %>%
  summarise(prop_mean = mean(prop),
            prop_2.5 = quantile(prop, .025),
            prop_97.5 = quantile(prop, .975))

#These last two plots visualize the uncertainty
res_mu1 <- res_mu %>%
  rowwise() %>%
  mutate(mean_mu = mean(c_across(X1:X240)),
         sd_mu = sd(c_across(X1:X240))) %>%
  select(day_of_year, log_flow, log_wq_orig, mean_mu, sd_mu) %>%
  ungroup()

res_mu_flow <- res_mu1 %>%
  group_by(log_flow) %>%
  summarise(min_mu = min(mean_mu),
            max_mu = max(mean_mu),
            mu_mean = mean(mean_mu))

res_mu_day <- res_mu1 %>%
  group_by(day_of_year) %>%
  summarise(min_mu = min(mean_mu),
            max_mu = max(mean_mu),
            mu_mean = mean(mean_mu))

res_y_flow <- res_y %>%
  rowwise() %>%
  group_by(log_flow) %>%
  summarise(mu_mean=1, 
            y2.5 = quantile(c_across(X1:X240), .025),
            y97.5 = quantile(c_across(X1:X240), .975)) %>%
  select(log_flow, mu_mean, y2.5, y97.5) %>%
  ungroup()

res_y_day <- res_y %>%
  rowwise() %>%
  group_by(day_of_year) %>%
  summarise(mu_mean=1, 
            y2.5 = quantile(c_across(X1:X240), .025),
            y97.5 = quantile(c_across(X1:X240), .975)) %>%
  select(day_of_year, mu_mean, y2.5, y97.5) %>%
  ungroup()

flow_por_temp <- flow_por %>%
  mutate(mu_mean=1)

ggplot(res_mu_flow, aes(10^log_flow, 10^mu_mean)) +
  geom_ribbon(data=res_y_flow, aes(x=10^log_flow, ymin=10^y2.5, ymax=10^y97.5), fill="dark gray", alpha=.5) +
  geom_ribbon(data=res_mu_flow, aes(ymin=10^min_mu, ymax=10^max_mu), fill="light blue") +
  geom_point(data=flow_por_temp, aes(10^log_flow, wq), size=2) +
  geom_line() +
  scale_x_log10(limits=c(1, 100)) +
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal() +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22))
#literature reviews say that TOC should not slope downward at high flows
#would changing the GAM to include multiple sites help find the right shape?

ggplot(res_y_day, aes(day_of_year, 10^mu_mean)) +
  #geom_ribbon(data=data_y_day, aes(x=day_of_year, ymin=10^y2.5, ymax=10^y97.5), fill="dark gray", alpha=.5) +
  geom_ribbon(data=res_mu_day, aes(ymin=10^min_mu, ymax=10^max_mu), fill="light blue") +
  geom_point(data=flow_por_temp, aes(day_of_year, wq), size=2) +
  geom_line() +
  scale_y_continuous(limits=c(NA, 50)) +
  xlab("Day of year") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal() +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22))
#very flat, is including day of year as a predictor a problem, or ok?

#############################################################################
#Does it make sense to have a calibration and validation dataset?
#Now looking at validation dataset

ver1 <- ver0 %>%
  mutate(day_of_year = yday,
         log_wq_ver = log_wq,
         log_flow_orig = log_flow,
         log_flow = round(log_flow, 1)) %>%
  select(day_of_year, log_flow, log_wq_ver)

res_y_val <- res_y %>%
  left_join(ver1, by=c("day_of_year", "log_flow"))
#lines with a log_wq_orig value have X1...X253 all identical

ggplot(res_mu_flow, aes(10^log_flow, 10^mu_mean)) +
  geom_ribbon(data=res_y_flow, aes(x=10^log_flow, ymin=10^y2.5, ymax=10^y97.5), fill="dark gray", alpha=.5) +
  geom_ribbon(data=res_mu_flow, aes(ymin=10^min_mu, ymax=10^max_mu), fill="light blue") +
  geom_point(data=flow_por_temp, aes(10^log_flow, wq), size=2) +
  geom_point(data=ver1, aes(10^log_flow, 10^log_wq_ver), size=2, color="orange") +
  geom_line() +
  scale_x_log10(limits=c(1, 100)) +
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal() +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22))

ggplot(res_y_day, aes(day_of_year, 10^mu_mean)) +
  #geom_ribbon(data=data_y_day, aes(x=day_of_year, ymin=10^y2.5, ymax=10^y97.5), fill="dark gray", alpha=.5) +
  geom_ribbon(data=res_mu_day, aes(ymin=10^min_mu, ymax=10^max_mu), fill="light blue") +
  geom_point(data=flow_por_temp, aes(day_of_year, wq), size=2) +
  geom_point(data=ver1, aes(day_of_year, 10^log_wq_ver), size=2, color="orange") +
  geom_line() +
  scale_y_continuous(limits=c(NA, 50)) +
  xlab("Day of year") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal() +
  theme(axis.text=element_text(size=22), axis.title=element_text(size=22))