library(tidyverse)
library(lubridate)
library(mgcv)
library(gratia)
library(jagsUI)

#How does total organic carbon (TOC) change with streamflow and day of year?
#What is the probability of TOC exceeding a given threshold?
#Predict future TOC values, exceedances, and uncertainty

#Using a generalized additive model for flexible relationships
#Using Bayesian analysis to estimate uncertainty

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
#I need to log-transform TOC and streamflow, even when using a GAM?

#Test out GAM
#Calibrate model with 2/3rds of the data
#Validate with 1/3rd of the data
set.seed(1) #so that I will have the same number of cal vs. ver values each time I run
data0$cal_ver <- rbinom(n=length(data0$date), size=1, prob=.66)
length(data0$cal_ver[data0$cal_ver==1])/length(data0$cal_ver)
length(data0$cal_ver[data0$cal_ver==1])

cal0 <- data0 %>%
  filter(cal_ver==1)
mod <- gam(log_wq ~ s(log_flow, bs="tp", k=4) + s(yday, bs="cc", k=4), data=cal0)
pred_wq <- predict(mod, newdata=cal0)
sim_wq <- simulate(mod, nsim=1, data=cal0) 
#simulate() gets the predicted values and then randomly draws from their probability distribution, could read docs to find out how
cal0$log_wq_pred <- c(pred_wq)
cal0$log_wq_sim <- sim_wq[,1]

ggplot(cal0, aes(flow, wq)) +
  geom_point(shape=1) +
  geom_point(data=cal0, aes(flow, 10^log_wq_sim), color="red", shape=1) +
  geom_point(data=cal0, aes(flow, 10^log_wq_pred), color="red")

ver0 <- data0 %>%
  filter(cal_ver==0)
pred_wq <- predict(mod, newdata=ver0)
sim_wq <- simulate(mod, nsim=1, data=ver0)
ver0$log_wq_pred <- c(pred_wq)
ver0$log_wq_sim <- sim_wq[,1]

ggplot(cal0, aes(flow, wq)) +
  geom_point(shape=1) +
  geom_point(data=cal0, aes(flow, 10^log_wq_sim), color="red", shape=1) +
  geom_point(data=cal0, aes(flow, 10^log_wq_pred), color="red") +
  geom_point(data=ver0, aes(flow, 10^log_wq_sim), color="blue", shape=1) +
  geom_point(data=ver0, aes(flow, 10^log_wq_pred), color="blue")
#the GAM looks reasonable, both the estimated mean and simulated values

ggplot(cal0, aes(wq, 10^log_wq_pred)) +
  geom_point(color="red") +
  geom_point(data=ver0, aes(wq, 10^log_wq_pred), color="blue") +
  geom_abline(aes(slope=1, intercept=0), linetype="dashed")

cor(cal0$wq, 10^cal0$log_wq_pred)^2 #~.86 for the calibration period
cor(ver0$wq, 10^ver0$log_wq_pred)^2 #between ~.59 - .89 for the validation period

top_sum <- 0
bot_sum <- 0
for (i in 1:length(cal0)) {
  top <- (cal0$log_wq[i] - cal0$log_wq_pred[i])^2 #using log values, makes differences count more at lower end of TOC range than they otherwise would, upper end counts less than it otherwise would 
  bot <- (cal0$log_wq[i] - mean(cal0$log_wq))^2
  
  top_sum <- top_sum + top
  bot_sum <- bot_sum + bot
}

1 - top_sum/bot_sum #~0.94 for the calibration period NSE

top_sum <- 0
bot_sum <- 0
for (i in 1:length(ver0)) {
  top <- (ver0$log_wq[i] - ver0$log_wq_pred[i])^2
  bot <- (ver0$log_wq[i] - mean(ver0$log_wq))^2
  
  top_sum <- top_sum + top
  bot_sum <- bot_sum + bot
}

1 - top_sum/bot_sum #~0.87 for the validation period NSE


################################################################################
#This section is just organizing the flow data

#Additional flow data (past) and future flow
all_flow <- read.csv("Data/USGS_flow/pH Hillsborough R near Zephyrhills.csv")
jason_flow <- read.csv("Data/Jason_flow/Chang data business as usual.csv")
jason_flow <- jason_flow %>%
  mutate(date = as.Date(date)) #future1 is for some reason missing the very last day
# NOTE TO SELF: update to use this flow data later

flow_por <- all_flow %>%
  mutate(date = as.Date(Date),
         flow = Q,
         log_flow = log10(Q)) %>%
  filter(date>=min(data0$date) & date<=max(data0$date)) %>%
  select(date, flow, log_flow) %>%
  left_join(cal0) %>% #change to data0 if using full dataset
  mutate(row = row_number(),
         day_of_year = yday(date),
         per="Past",
         mod="USGS") %>%
  select(per, mod, date, flow, log_flow, wq, log_wq, day_of_year)

jason_flow1 <- jason_flow %>%
  mutate(flow = flow/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         day_of_year = yday(date),
         per=period,
         mod=scenario) %>%
  select(per, mod, date, flow, log_flow, wq, log_wq, day_of_year)

data_by_day <- flow_por %>%
  bind_rows(jason_flow1) %>%
  mutate(log_flow = round(log_flow, digits=1)) %>%
  mutate(mod = str_remove(mod, "rcp85_")) %>%
  filter(!str_detect(mod, "Priestley") & !str_detect(mod, "Hargreaves"))

min_vals <- data_by_day %>%
  filter(flow>0) %>%
  group_by(per, mod) %>%
  summarise(min = min(flow))

data_by_day_zero <- data_by_day %>%
  filter(flow==0) %>%
  left_join(min_vals, by=c("per", "mod")) %>%
  mutate(flow = min,
         log_flow = log10(min)) %>%
  mutate(log_flow = round(log_flow, digits=1)) %>%
  select(-min)

data_by_day_nonzero <- data_by_day %>%
  filter(flow!=0)

data_by_day <- data_by_day_zero %>%
  bind_rows(data_by_day_nonzero)

unique(data_by_day$per)
unique(data_by_day$mod)
ggplot(data_by_day, aes(date, flow, color=mod)) +
  geom_line() +
  scale_y_log10(breaks=c(.1, 1, 10, 100, 1000), labels=c(".1", "1", "10", "100", "1000")) +
  #scale_color_manual(values=c("#000000", "#2C621B", "#871E03"), name="Dataset/GCM") +
  xlab("Date") +
  ylab("Daily mean streamflow (cms)") +
  facet_wrap(~ per, scales="free_x", ncol=1) +
  theme_minimal() #+
  # guides(color=guide_legend(override.aes=list(linewidth=5))) +
  # theme(axis.text=element_text(size=22), 
  #       axis.text.x=element_text(angle=45, hjust=1),
  #       axis.title=element_text(size=22),
  #       axis.title.x=element_blank(),
  #       legend.text=element_text(size=22),
  #       legend.title=element_text(size=22),
  #       legend.key.size=unit(4, "line"),
  #       #legend.position="none",
  #       strip.text=element_blank())
#ggsave("Plot orig flow.png", width=3.5, height=3.5*5)
#ggsave("Plot orig flow legend.png", width=4.5, height=3.5*5)

min_obs <- min(data_by_day$log_flow[!is.na(data_by_day$log_wq)])
min <- min(data_by_day$log_flow)
max_obs <- max(data_by_day$log_flow[!is.na(data_by_day$log_wq)])
max <- max(data_by_day$log_flow)

data1 <- data.frame(day_of_year = rep(1:366, each=78), log_flow = rep(seq(from=min, to=max, by=.1), 366))
data1 <- data1 %>%
  mutate(log_flow = round(log_flow, digits=1))

data_to_join <- data_by_day %>%
  filter(!is.na(wq)) %>%
  select(log_flow, log_wq, day_of_year) %>%
  group_by(log_flow, day_of_year) %>%
  summarise(log_wq = mean(log_wq, na.rm=TRUE)) %>%
  ungroup() #had to average duplicate day/flow combos

data2 <- data1 %>%
  left_join(data_to_join, by=c("day_of_year", "log_flow"))
#data2 will be the input data frame for jagam

################################################################################

#Now for Bayesian version
#I don't know how to get estimates for new values of streamflow and day of year
#I would need to multiply the design matrix by the estimated parameters
#tmp_model$jags.data$X %*% t(mcmc[,1:6]) #calculated mu
#...but how to make the design matrix? (other than letting jagam do it, I don't know)
#So, letting jagam make the design matrix, 
#including the full range/equally spaced values of streamflow and year
#also, I am only using the calibration dataset...but I might as well use the full dataset?

tmp_jags_code <- "Scripts/tmp_jags_code 2024-02-23.R" #where the jags code file will be written
tmp_model <- jagam(log_wq ~ s(log_flow, bs="tp", k=4) + s(day_of_year, bs="cc", k=4), data=data2, file=tmp_jags_code, diagonalize=FALSE, na.action=na.pass)
#haven't made any changes to the default jagam suggestions,
#except to change NA to .016 (.016 is what jagam recommended when I didn't have missing values of wq)

#Set parameters to monitor
params = c("b", "tau", "mu", "y") #b are the parameters, tau is precision, mu is expected values, y is estimated values

#Run model
mod <- jags(model.file = "Scripts/tmp_jags_code 2024-02-23.R",
            parameters.to.save = params,
            data = tmp_model$jags.data, #makes the design matrix for me?
            n.chains = 3, #number of chains
            n.burnin = 2000, #number of iterations to discard as burn-in
            n.iter = 10000, #number of iterations
            n.thin = 100, #interval to thin
            DIC = TRUE)

mcmc <- do.call('rbind', mod$samples)
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
#does it look like my burn-in isn't long enough? (1000 wasn't long enough)

#matching estimated vals (y) with calibration dataset
mcmc[1,57104]
mcmc[1,57103] #end of y
57103-28548+1
mcmc[1,28556] #start of y
mcmc[1,28555] #end of mu
28555-28548+1
mcmc[1,8] #start of mu

mcmc_yt <- data.frame(t(mcmc[,28556:57103]))
res_y <- data2 %>%
  mutate(log_wq_orig = log_wq) %>%
  select(-log_wq) %>%
  bind_cols(mcmc_yt)
#why are the estimated values (X1:X240) exactly the same as the input dataset values (log_wq_orig)?

#matching estimated means (mu) with calibration dataset
mcmc_mut <- data.frame(t(mcmc[,8:28555]))
res_mu <- data2 %>%
  mutate(log_wq_orig = log_wq) %>%
  select(-log_wq) %>%
  bind_cols(mcmc_mut)

ggplot(res_y, aes(10^log_flow, 10^log_wq_orig)) +
  geom_point(data=res_y, aes(10^log_flow, 10^X1), shape=1, color="green") +
  geom_point(data=res_mu, aes(10^log_flow, 10^X1), color="red") +
  geom_point(shape=1) +
  scale_x_log10()
#much bigger range than my input dataset...

#Displaying results/exceedances
#Predict exceedances (TOC above 8)
#for each value of actual predicted flow, what is the distribution of wq
#the prob of being above 8...

#bind to data by day...
data_by_day1 <- data_by_day %>%
  left_join(res_y, by=c("day_of_year", "log_flow")) %>%
  mutate(mod_per = paste0(mod, " ", per, sep=""))
unique(data_by_day1$mod_per)

#get some kind of mean and range or distribution
i="NLDAS-2_PM Past"
dist_df <- data.frame()
for (i in unique(data_by_day1$mod_per)) {
  
  sub <- data_by_day1 %>%
    filter(mod_per==i)
  
  sub_list <- data.frame(mod_per=i, vals=unlist(list(sub[,10:249])))
  sub_list$cume_dist <- cume_dist(sub_list$vals)
  sub_list$per_exceed <- 1 - sub_list$cume_dist
  sub_list$per_rank <- percent_rank(sub_list$vals)
  sub_list$quantilemin <- quantile(sub_list$vals, .01)
  sub_list$quantile50 <- quantile(sub_list$vals, .5)
  sub_list$quantilemax <- quantile(sub_list$vals, .995)
  
  dist_df <- dist_df %>%
    bind_rows(sub_list)
  
} #takes some time

threshold <- dist_df %>%
  filter(vals>log10(7.95) & vals<log10(8.05)) %>% #because filtering by vals==log10(8) wasn't working, rounding errors
  group_by(mod_per) %>%
  summarise(thresh=mean(per_exceed))

threshold <- threshold %>%
  mutate(per = case_when(str_detect(mod_per, "Past") ~ "Past",
                         str_detect(mod_per, "Future1") ~ "Future1",
                         str_detect(mod_per, "Future2") ~ "Future2")) %>%
  mutate(mod0 = str_remove(mod_per, " Past")) %>%
  mutate(mod1 = str_remove(mod0, " Future1")) %>%
  mutate(mod = str_remove(mod1, " Future2"))

threshold <- threshold %>%
  mutate(mod2 = case_when(mod=="NLDAS-2_PM" ~ "dashed",
                          TRUE ~ "solid")) %>%
  mutate(mod = factor(mod, ordered=TRUE, levels=c("IHM_original",
                                                  "bcc_csm_PM",
                                                  "BNU_ESM_PM",
                                                  "MIROC_ESM_PM",
                                                  "MPI_ESM_LR_PM",
                                                  "MRI_CGCM3_PM",
                                                  "NorESM1_M_PM",
                                                  "GFDL_CM3_PM",
                                                  "GFDL_ESM2G_PM",
                                                  "USGS",
                                                  "NLDAS-2_PM"))) %>%
  mutate(per = factor(per, ordered=TRUE, levels=c("Past", "Future1", "Future2")))

dist_df_smaller <- dist_df %>%
  mutate(use_row = rep(1:40,times=1623606)) %>%
  filter(use_row==1) %>%
  mutate(per = case_when(str_detect(mod_per, "Past") ~ "Past",
                         str_detect(mod_per, "Future1") ~ "Future1",
                         str_detect(mod_per, "Future2") ~ "Future2")) %>%
  mutate(mod0 = str_remove(mod_per, " Past")) %>%
  mutate(mod1 = str_remove(mod0, " Future1")) %>%
  mutate(mod = str_remove(mod1, " Future2"))

unique(dist_df_smaller$per)
unique(threshold$mod)
class(dist_df_smaller$mod)
dist_df_smaller <- dist_df_smaller %>%
  mutate(mod = factor(mod, ordered=TRUE, levels=c("IHM_original",
                                                  "bcc_csm_PM",
                                                     "BNU_ESM_PM",
                                                     "MIROC_ESM_PM",
                                                     "MPI_ESM_LR_PM",
                                                     "MRI_CGCM3_PM",
                                                     "NorESM1_M_PM",
                                                  "GFDL_CM3_PM",
                                                  "GFDL_ESM2G_PM",
                                                     "USGS",
                                                     "NLDAS-2_PM"))) %>%
  mutate(mod2 = case_when(mod=="NLDAS-2_PM" ~ "dashed",
                          TRUE ~ "solid")) %>%
  mutate(per = factor(per, ordered=TRUE, levels=c("Past", "Future1", "Future2")))

ggplot(dist_df_smaller, aes(per_exceed*100, 10^vals, group=mod, color=mod)) +
  geom_line() +
  geom_segment(data=threshold, aes(x=thresh*100, y=-Inf, xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod)) +
  geom_hline(yintercept=10^(log10(8)), linetype="solid", color="black") +
  scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
  scale_y_continuous(limits=c(.1, 60)) + #specific to analyte
  scale_colour_manual(values=c("NLDAS-2_PM"="#984ea3", "GFDL_CM3_PM"="#7fc97f",
                               "BNU_ESM_PM"="#b3b3b3", "GFDL_ESM2G_PM"="#fdc086",
                               "MIROC_ESM_PM"="#b3b3b3", "bcc_csm_PM"="#b3b3b3",
                               "MPI_ESM_LR_PM"="#b3b3b3",
                               "NorESM1_M_PM"="#b3b3b3", "USGS"="#000000",
                               "IHM_original"="#b3b3b3", "MRI_CGCM3_PM"="#b3b3b3")) +
  #scale_linetype_manual(values=c("dashed"="dashed", "solid"="solid")) +
  # scale_colour_manual(values=c("NLDAS-2_PM"="#666666", "GFDL_CM3_PM"="#1b9e77",
  #                              "BNU_ESM_PM"="#d95f02", "GFDL_ESM2G_PM"="#7570b3", 
  #                              "MIROC_ESM_PM"="#e6ab02", "bcc_csm_PM"="#e7298a",   
  #                              "MPI_ESM_LR_PM"="#66a61e",  
  #                              "NorESM1_M_PM"="#a6761d", "USGS"="black", 
  #                              "IHM_original"="#666666", "MRI_CGCM3_PM"="#386cb0")) +
  ylab("Total organic carbon (mg/l)") +
  xlab("Percent exceedance (%)") +
  # coord_flip() +
  facet_wrap(~per, nrow=1) +
  theme_minimal() +
  theme(#axis.text=element_text(size=22),
        #axis.title=element_text(size=22),
        #strip.text=element_text(size=22),
        #legend.position="none",
        legend.title=element_blank())
#this plot takes a minute or two
#OMITS THE HIGHEST 0.1% OF VALUES
#and TOC values outside of 0.1 to 60 mg/l
ggsave("All GCMs.png", width=6.5, height=2.9)
#ggsave("All GCMs flip.png", width=6.5, height=3)


#Get credible intervals of exceedances
#A better way to do this would be to change the GAM
#So that the GAM response is the percent exceedance, rather than just the wq value
#(but unsure how to define it correctly)
ci_thresh <- data.frame()
i="Past"
j="IHM_original"
k=10
for (i in unique(data_by_day1$per)) {
  for (j in unique(data_by_day1$mod)) {
    for (k in 10:249) {
    
    sub <- data_by_day1 %>%
      filter(per==i & mod==j) %>%
      select(all_of(k))
    
    colnames(sub) <- "vals"
    
    abv_8 <- sub %>%
      filter(vals>log10(8))
    
    abv_8 <- length(abv_8$vals)/length(sub$vals)
    new_df <- data.frame(per=i, mod=j, iter=k, prop=abv_8)
    
    ci_thresh <- ci_thresh %>%
      bind_rows(new_df)
    }
  }
}#this loop takes a couple minutes

#This table is the credible interval results
#For percent exceedance over 8 mg/l TOC, and uncertainty around that

ci_thresh_summary <- ci_thresh %>%
  group_by(per, mod) %>%
  summarise(prop_mean = mean(prop, na.rm=TRUE),
            prop_2.5 = quantile(prop, .025, na.rm=TRUE),
            prop_97.5 = quantile(prop, .975, na.rm=TRUE))
write.csv(ci_thresh_summary, "threshold summary.csv", row.names = FALSE)

ci_thresh_summary <- ci_thresh_summary %>%
  filter(!is.na(prop_97.5)) %>%
  mutate(per = factor(per, ordered=TRUE, levels=c("Past", "Future1", "Future2"))) %>%
  mutate(mod = factor(mod, ordered=TRUE, levels=c("IHM_original",
                                                  "bcc_csm_PM",
                                                  "BNU_ESM_PM",
                                                  "MIROC_ESM_PM",
                                                  "MPI_ESM_LR_PM",
                                                  "MRI_CGCM3_PM",
                                                  "NorESM1_M_PM",
                                                  "GFDL_CM3_PM",
                                                  "GFDL_ESM2G_PM",
                                                  "USGS",
                                                  "NLDAS-2_PM")))

ggplot(ci_thresh_summary, aes(mod, prop_mean*100, color=mod)) +
  geom_col(fill=NA) +
  geom_errorbar(aes(ymin=prop_2.5*100, ymax=prop_97.5*100), width=0) +
  scale_color_manual(values=c("NLDAS-2_PM"="#984ea3", "GFDL_CM3_PM"="#7fc97f",
                               "BNU_ESM_PM"="#b3b3b3", "GFDL_ESM2G_PM"="#fdc086",
                               "MIROC_ESM_PM"="#b3b3b3", "bcc_csm_PM"="#b3b3b3",
                               "MPI_ESM_LR_PM"="#b3b3b3",
                               "NorESM1_M_PM"="#b3b3b3", "USGS"="#000000",
                               "IHM_original"="#b3b3b3", "MRI_CGCM3_PM"="#b3b3b3")) +
  facet_wrap(~per, scales="free_x") +
  ylab("Threshold exceedances (%)\n(TOC values above 8 mg/l)") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1),
        axis.title.x=element_blank(),
        legend.title=element_blank())
ggsave("exceedances bar.png", width=6.5, height=2.9)

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