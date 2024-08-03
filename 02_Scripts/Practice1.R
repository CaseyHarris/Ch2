library(tidyverse)
library(lubridate)
library(mgcv)
library(jagsUI)
library(gratia)

#How does water quality change with streamflow, or streamflow and temperature?
#What is the probability of water quality exceeding relevant thresholds?
#Report future distributions of water quality and uncertainty

#Using a generalized additive model for flexible relationships
#Using Bayesian analysis to provide credible intervals (interval that contains X% of values)
#which are more easily interpretable than frequentist confidence intervals for making decisions
#also useful for hierarchical models, constraining outcomes, examining the posterior in various ways

#Look at actual data
data0 <- read.csv("01_Data/actual data.csv")
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

#Use a GAM to simulate some data
mod0 <- gam(wq ~ s(log_flow, bs="tp", k=4), family="scat", data=data0)

pred_mean <- predict.gam(mod0, newdata=data0)
data0$pred_mean <- c(pred_mean)

data_for_sim <- data0 %>%
  select(log_flow)

sim_wq <- simulate(mod0, nsim=1000, data=data0)
#simulate() gets the predicted values and then randomly draws from their probability distribution, could read docs to find out how

sim_summary <- data.frame()
for (i in 1:132) {
  dat <- sim_wq[i,]
  sim_05 <- quantile(dat, .01)
  sim_95 <- quantile(dat, .99)
  sim_mean <- mean(dat)
  sim_median <- median(dat)
  
  sim_summary_new <- data.frame(sim_05, sim_95, sim_mean, sim_median)
  sim_summary <- sim_summary %>%
    bind_rows(sim_summary_new)
}

data1 <- data0 %>%
  mutate(sim_wq = sim_wq[,1]) %>%
  bind_cols(sim_summary)

ggplot(data1, aes(flow, wq)) +
  geom_point(shape=1, size=2) +
  geom_point(data=data1, aes(flow, pred_mean), color="orange") +
  geom_point(data=data1, aes(flow, sim_wq), color="red") +
  geom_point(data=data1, aes(flow, sim_mean), color="blue") +
  geom_errorbar(data=data1, aes(ymin=sim_05, ymax=sim_95))

################################################################################

#Now for Bayesian version
#I don't know how to get estimates for new values of streamflow and day of year
#I would need to multiply the design matrix by the estimated parameters
#tmp_model$jags.data$X %*% t(mcmc[,1:6]) #calculated mu
#...but how to make the design matrix? (other than letting jagam do it, I don't know)
#So, letting jagam make the design matrix

tmp_jags_code <- "02_Scripts/tmp_jags_code 2024-06-13 unedited.R" #where the jags code file will be written
tmp_model_obs <- jagam(wq ~ s(log_flow, bs="tp", k=4), data=data1, file=tmp_jags_code, family=gaussian(link="identity"))

#have to change jags file to scat
#haven't made any other changes to the default jagam suggestions,

#Set parameters to monitor
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
#don't know why all the y's are the same, leaving out y's
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
  mutate(y = z/sqrt(S))

mcmc_longer_summary <- mcmc_longer %>%
  group_by(flow) %>%
  summarise(y_05 = quantile(y, .05),
            y_95 = quantile(y, .95),
            y_50 = quantile(y, .5),
            y_mean = mean(y))

unique(mcmc_longer$flow)
ggplot(filter(mcmc_longer, y<10000), aes(y, group=flow)) +
  geom_density()
#7:138 mu
#139:270 S
#271:402 z

##ask why I simulate ridiculously high values
##can we control how high it goes

ggplot(mcmc_longer_summary, aes(flow, y_50)) +
  geom_errorbar(data=mcmc_longer_summary, aes(flow, ymin=y_05, ymax=y_95), color="red") +
  geom_point(color='red') + #modeled wq
  geom_point(data=data1, aes(flow, wq)) + #original wq
  geom_point(data=mcmc_longer, aes(flow, mu), color="blue") +
  xlab("Streamflow (cms)") +
  ylab("Total organic carbon (mg/l)") +
  theme_minimal()
rm(mcmc_longer)
#mu  are good, but est values see to be systematically too high

##try to estimate y's
res_df <- data.frame()
n_i <- length(mcmc_df$tau)
n_j <- length(data1$wq)
tau_col <- which(colnames(mcmc_df)=="tau")
k_col <- which(colnames(mcmc_df)=="k")
#i=10
#j=10
rm(i)
rm(j)
for (i in 1:n_i) {
  
  tau_i <- mcmc[i, tau_col]
  k_i <- mcmc[i, k_col]
  
  for (j in 1:n_j) {
    
    mu_i_j <- mcmc[i, j+k_col]
    
    # x <- rnorm(1, mu_i_j, tau_i)
    # s <- rgamma(1, k_i/2, k_i/2)
    # y_i_j <- x/sqrt(s)
    
    res_df_new <- data.frame(b1=mcmc[i,1], b2=mcmc[i,2], b3=mcmc[i,3], b4=mcmc[i,4], 
                             tau=tau_i, k=k_i,
                             flow_orig=data1$flow[j],
                             yday_orig=data1$yday[j],
                             mu=mu_i_j,
                             y=y_i_j,
                             wq_orig=data1$wq[j])
    
    res_df <- res_df %>%
      bind_rows(res_df_new)
    
  }
}

res_df1 <- res_df %>%
  group_by(flow_orig, yday_orig, wq_orig) %>%
  summarise(y_mean = mean(y),
            y_median = median(y),
            y_05 = quantile(y, .05),
            y_95 = quantile(y, .95))
  # summarise(y_mean = 10^mean(log10(y)),
  #           y_05 = 10^quantile(log10(y), .05, na.rm=TRUE),
  #           y_95 = 10^quantile(log10(y), .95, na.rm=TRUE),
  #           n = n())

ggplot(res_df1, aes(flow_orig, wq_orig)) +
  geom_point() +
  geom_point(aes(flow_orig, y_mean), color="blue") +
  geom_errorbar(aes(ymin=y_05, ymax=y_95))

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