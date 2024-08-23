library(ggh4x)
library(scales)
library(tidyverse)

#Working with summaries

jags_cal_val <- read.csv("JAGS cal val.csv")
jags_summary <- read.csv("JAGS summary.csv")

gam_cal_val <- read.csv("GAM cal val.csv")
gam_summary <- read.csv("GAM summary.csv")

unique(jags_summary$in_or_out)
jags_min_max <- jags_summary %>%
  filter(in_or_out=="in range") %>%
  group_by(title) %>%
  summarise(min_Q = min(Q_cfs_log_round),
            max_Q = max(Q_cfs_log_round))

jags_summary1 <- jags_summary %>%
  left_join(jags_min_max, by="title") %>%
  filter(Q_cfs_log_round==min_Q | Q_cfs_log_round==max_Q) %>%
  mutate(min_max = case_when(Q_cfs_log_round==min_Q ~ "min",
                             Q_cfs_log_round==max_Q ~ "max",
                             TRUE ~ NA_character_),
         mu_mean_sub = mu_mean,
         mu_025_sub = mu_025,
         mu_975_sub = mu_975,
         y_mean_sub = y_mean,
         y_025_sub = y_025,
         y_975_sub = y_975) %>%
  select(title, mu_mean_sub, mu_025_sub, mu_975_sub, y_mean_sub, y_025_sub, y_975_sub, min_max)

jags_summary2 <- jags_summary %>%
  mutate(min_max = case_when(Q_cfs_log_round<min_flow_sampled ~ "min",
                             Q_cfs_log_round>max_flow_sampled ~ "max",
                             TRUE ~ NA_character_))

jags_summary3 <- jags_summary2 %>%
  left_join(jags_summary1, by=c("title", "min_max")) %>%
  mutate(mu_mean_use = case_when(min_max=="min" | min_max=="max" ~ mu_mean_sub,
                                 TRUE ~ mu_mean),
         mu_025_use = case_when(min_max=="min" | min_max=="max" ~ mu_025_sub,
                                 TRUE ~ mu_025),
         mu_975_use = case_when(min_max=="min" | min_max=="max" ~ mu_975_sub,
                                TRUE ~ mu_975),
         y_mean_use = case_when(min_max=="min" | min_max=="max" ~ y_mean_sub,
                                TRUE ~ y_mean),
         y_025_use = case_when(min_max=="min" | min_max=="max" ~ y_025_sub,
                                TRUE ~ y_025),
         y_975_use = case_when(min_max=="min" | min_max=="max" ~ y_975_sub,
                                TRUE ~ y_975))

min_max_est <- jags_summary3 %>%
  group_by(title) %>%
  summarise(min_est = min(Q_cfs_log_round),
            max_est = max(Q_cfs_log_round)) %>%
  ungroup()

jags_summary4 <- jags_summary3 %>%
  left_join(min_max_est, by="title") %>%
  filter((Q_cfs_log_round>=min_flow_sampled & Q_cfs_log_round<=max_flow_sampled) |
           (Q_cfs_log_round==min_est | Q_cfs_log_round==max_est)) 

i="Fluoride Alafia R at Lithia"
for (i in unique(jags_summary4$title)) {
  
  use0 <- jags_summary4 %>%
    filter(title==i)
  
  use_cal_val <- jags_cal_val %>%
    filter(title==i, cal_or_val!="neither")
  
  # x_min_samp = min(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_flow_sampled])
  # x_min_samp_plus = min(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_flow_sampled]) - .25
  # x_min_est = use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_est]
  # x_max_samp = max(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_flow_sampled])
  # x_max_samp_plus = max(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_flow_sampled]) + .5
  # x_max_est = use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_est]
  # 
  # jags_summary5 <- jags_summary4 %>%
  #   mutate(Q_cfs_log_round = case_when(Q_cfs_log_round==min_est ~ min_flow_sampled - .25,
  #                                      Q_cfs_log_round==max_est ~ max_flow_sampled + .5,
  #                                      TRUE ~ Q_cfs_log_round))
  # 
  # ggplot(filter(jags_summary5, title==i)) +
  #   geom_line(aes(exp(Q_cfs_log_round), y_025_use)) +
  #   geom_line(aes(exp(Q_cfs_log_round), y_975_use)) +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_mean_use), linetype="dashed") +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_025_use)) +
  #   geom_line(aes(exp(Q_cfs_log_round), mu_975_use)) +
  #   geom_vline(aes(xintercept=exp(x_min_samp)), linetype="dashed") +
  #   geom_vline(aes(xintercept=exp(x_max_samp)), linetype="dashed") +
  #   geom_point(data=use_cal_val[use_cal_val$cal_or_val=="Model calibration",], aes(exp(Q_cfs_log_round), wq_orig), shape=1, color="green") +
  #   geom_point(data=use_cal_val[use_cal_val$cal_or_val=="Model validation",], aes(exp(Q_cfs_log_round), wq_orig), shape=1, color="green") +
  #   geom_point(data=use_cal_val, aes(exp(Q_cfs_log_round), y_est_100), shape=1) +
  #   xlab("Flow (cfs)\non log10 scale with axis breaks") +
  #   ylab("Water quality (units)") +
  #   ggtitle(i) +
  #   theme_classic() +
  #   guides(x=guide_axis_truncated(trunc_lower=c(0, exp(x_min_samp)), trunc_upper = c(exp(x_min_samp_plus), exp(x_max_samp)))) +
  #   annotate("text", y=-Inf, x=c(exp(x_min_samp_plus), exp(x_min_samp), exp(x_max_samp), exp(x_max_samp_plus)), label = "I") + 
  #   scale_x_log10(breaks=c(exp(x_min_samp_plus), exp(x_min_samp), 50, 100, 200, 500, 1000, exp(x_max_samp), exp(x_max_samp_plus)), 
  #                 labels=c(round(exp(x_min_est),.1), round(exp(x_min_samp),.1), 50, 100, 200, 500, 1000, round(exp(x_max_samp),.1),round(exp(x_max_est),.1))) +
  #   coord_cartesian(clip = "off") +
  #   theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1, size=10), axis.text.y=element_text(size=10), axis.title=element_text(size=10),
  #         plot.title=element_text(size=10))
  # ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Model range/range ", i, " trans.png", sep=""), width=4, height=3)
  # 
  x_min_samp = exp(min(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_flow_sampled]))
  x_min_samp_plus = exp(min(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_flow_sampled])) - 100
  x_min_est = exp(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$min_est])
  x_max_samp = exp(max(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_flow_sampled]))
  x_max_samp_plus = exp(max(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_flow_sampled])) + 100
  x_max_est = exp(use0$Q_cfs_log_round[use0$Q_cfs_log_round==use0$max_est])
  
  jags_summary6 <- jags_summary4 %>%
    mutate(Q_cfs_log_round = case_when(Q_cfs_log_round==min_est ~ -50, # log(exp(min_flow_sampled) - 4),
                                       Q_cfs_log_round==max_est ~ exp(max_flow_sampled) + 100,
                                       TRUE ~ exp(Q_cfs_log_round)))
  ggplot(filter(jags_summary6, title==i)) +
    geom_line(aes(Q_cfs_log_round, y_025_use)) +
    geom_line(aes(Q_cfs_log_round, y_975_use)) +
    geom_line(aes(Q_cfs_log_round, mu_mean_use), linetype="dashed") +
    geom_line(aes(Q_cfs_log_round, mu_025_use)) +
    geom_line(aes(Q_cfs_log_round, mu_975_use)) +
    geom_vline(aes(xintercept=x_min_samp), linetype="dashed") +
    geom_vline(aes(xintercept=x_max_samp), linetype="dashed") +
    geom_point(data=use_cal_val[use_cal_val$cal_or_val=="Model calibration",], aes(exp(Q_cfs_log_round), wq_orig), shape=1, color="green") +
    geom_point(data=use_cal_val[use_cal_val$cal_or_val=="Model validation",], aes(exp(Q_cfs_log_round), wq_orig), shape=1, color="green") +
    geom_point(data=use_cal_val, aes(exp(Q_cfs_log_round), y_est_100), shape=1) +
    xlab("Flow (cfs)\n*axis breaks are not to scale*") +
    ylab("Water quality (units)") +
    ggtitle(i) +
    theme_classic() +
    guides(x=guide_axis_truncated(trunc_lower=c(-Inf, x_min_samp), trunc_upper = c(x_min_samp_plus, x_max_samp))) +
    annotate("text", y=-Inf, x=c(x_min_samp_plus, x_min_samp, x_max_samp, x_max_samp_plus), label = "I") + 
    scale_x_continuous(breaks=c(x_min_samp_plus, x_min_samp, 100, 500, 1000, 1500, x_max_samp, x_max_samp_plus),
                       labels=c(round(x_min_est,.1), round(x_min_samp,.1), 100, 500, 1000, 1500, round(x_max_samp,.1),round(x_max_est,.1))) +
    coord_cartesian(clip = "off") +
    theme(axis.text.x=element_text(angle=90, vjust=.5, hjust=1, size=10), axis.text.y=element_text(size=10), axis.title=element_text(size=10),
          plot.title=element_text(size=10))
  ggsave(paste0("C:/Users/cshar/OneDrive/UF Personal/Ch2/Large files/Figures/Model range/range ", i, " untrans.png", sep=""), width=6, height=5)
  
}

#summary where out of range values are equal to the min or max sampled
#match up to historic and future values
#plot timeseries, highlight out of range values
#plot frequency
#percent of out of range values
