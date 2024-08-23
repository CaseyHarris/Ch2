library(tidyverse)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
wq_names_df <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris"))
unique(wq_names_df$wq_names)

max_min <- read.csv("03_Results/max_min.csv")
flow_Alaf_Hills <- read.csv("flow_Alaf_Hills.csv")
flow_Jason <- read.csv("flow_Jason1.csv")

i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names$wq_names) {
  
  title_use <- gsub(".csv", "", i)
  wq <- read.csv(paste0("01_Data/Orig wq and flow/WQ/", i, sep="")) #water quality data
  wq <- wq %>%
    mutate(Date = as.Date(Date),
           wq_orig = ConcAve) %>%
    select(Date, wq_orig)
  
  mcmc_summary <- read.csv(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y_per_cfs/", i, ".csv", sep=""))
  mcmc_summary_wider <- mcmc_summary %>%
    pivot_longer(cols=3:12) %>%
    mutate(y = value) %>%
    select(-name, -value) %>%
    arrange(Q_cfs_log_round)
  n_y <- length(mcmc_summary_wider$y[mcmc_summary_wider$Q_cfs_log_round==0])
  
  mcmc_summary_wider1 <- mcmc_summary_wider %>%
    mutate(id = rep(seq(from=1, to=n_y), times=length(unique(mcmc_summary_wider$Q_cfs_log_round)))) %>%
    pivot_wider(names_from=id, values_from=y) %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1))
  colnames(mcmc_summary_wider1) <- make.names(colnames(mcmc_summary_wider1))
  
  max_min1 <- max_min %>%
    filter(title==title_use)
  
  if (str_detect(title_use, "Alafia")) {
    flow_past <- flow_Alaf_Hills %>%
      filter(site=="Alafia") %>%
      select(Date, Q_cfs_log_round)
    flow_jason <- flow_Jason %>%
      filter(site=="Alafia")
  } else if (str_detect(title_use, "Morris")) {
    flow_past <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough") %>%
      select(Date, Q_cfs_log_round)
    flow_jason <- flow_Jason %>%
      filter(site=="Hillsborough")
  }
  
  wq_flow_past <- flow_past %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_wq_2010"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_wq_2010"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_wq_2010"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_wq_2010"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    left_join(mcmc_summary_wider1, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  wq_flow_past1 <- wq_flow_past %>%
    pivot_longer(cols=8:ncol(wq_flow_past)) %>%
    mutate(y = value,
           date=as.Date(Date),
           Chang_item=NA_integer_,
           Realiz_num=NA_integer_,
           scenario="USGS") %>%
    select(title, scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, y)
  
  wq_flow_jason <- flow_jason %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_wq_2010"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_wq_2010"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_wq_2010"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_wq_2010"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    filter(Chang_item==4 | Chang_item==101 | 
             Chang_item==107 | Chang_item==110 |
             Chang_item==653 | Chang_item==656 |
             Chang_item==677 | Chang_item==680) %>%
    left_join(mcmc_summary_wider1, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  wq_flow_jason1 <- wq_flow_jason %>%
    pivot_longer(cols=17:ncol(wq_flow_jason)) %>%
    mutate(y = value,
           date=as.Date(date)) %>%
    select(title, scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, y)
  
  both <- wq_flow_jason1 %>%
    bind_rows(wq_flow_past1) %>%
    mutate(Chang_item = case_when(is.na(Chang_item) ~ 0,
                                  TRUE ~ Chang_item))
  
  j=4
  dist_df <- data.frame()
  for (j in unique(both$Chang_item)) {
  
    sub <- both %>%
      filter(Chang_item==j)
  
    sub$cume_dist <- cume_dist(sub$y)
    sub$per_exceed <- 1 - sub$cume_dist
    sub$per_rank <- percent_rank(sub$y)
    sub$quantilemin <- quantile(sub$y, .025)
    sub$quantile50 <- quantile(sub$y, .5)
    sub$quantilemax <- quantile(sub$y, .975)
  
    dist_df <- dist_df %>%
      bind_rows(sub)
  
  }

  dist_df <- dist_df %>%
    mutate(model = case_when(Chang_item==0 ~ "USGS",
                             Chang_item==4 ~ "IHM_original",
                             Chang_item==101 ~ "NLDAS-2_Hargreaves",
                             Chang_item==107 ~ "GFDL_CM3_Hargreaves",
                             Chang_item==110 ~ "GFDL_ESM2G_Hargreaves",
                             Chang_item==653 ~ "GFDL_CM3_Hargreaves",
                             Chang_item==656 ~ "GFDL_ESM2G_Hargreaves",
                             Chang_item==677 ~ "GFDL_CM3_Hargreaves",
                             Chang_item==680 ~ "GFDL_ESM2G_Hargreaves",
                            TRUE ~ NA_character_)) %>%
    mutate(time = case_when(Chang_item==0 ~ "past",
                            Chang_item==4 ~ "past",
                            Chang_item==101 ~ "past",
                            Chang_item==107 ~ "past",
                            Chang_item==110 ~ "past",
                            Chang_item==653 ~ "2030-2060",
                            Chang_item==656 ~ "2030-2060",
                            Chang_item==677 ~ "2070-2100",
                            Chang_item==680 ~ "2070-2100",
                            TRUE ~ NA_character_))

  # threshold <- dist_df %>%
  #   filter(y>log(7.95) & y<log10(8.05)) %>%
  #   group_by(Chang_item, model, time) %>%
  #   summarise(thresh=mean(per_exceed))

  ggplot(dist_df, aes(per_exceed*100, y, group=model, color=model)) +
    #geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
    geom_line(size=2) +
    #geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
    scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    scale_y_continuous(limits=c(.1, 250)) + #specific to analyte
    scale_color_manual(values=c("USGS"="black", "IHM_original"="darkblue", "NLDAS-2_Hargreaves"="darkorange", "GFDL_CM3_Hargreaves"="#2C621B", "GFDL_ESM2G_Hargreaves"="#871E03")) +
    ylab("Water quality (units)") +
    xlab("Percent exceedance (%)") +
    facet_wrap(~time, nrow=1) +
    theme_minimal() +
    theme(axis.text=element_text(size=22),
          axis.title=element_text(size=22),
          strip.text=element_text(size=22),
          legend.position="none")
  ggsave(paste0("03_Results/Figures/Exceedance probability/", title, ".png", sep=""), width=10, height=5)

# #What about CI?
# ci_thresh <- data.frame()
# i="CM3_1"
# j=10
# for (i in unique(data_by_day1$per)) {
#   for (j in 10:249) {
#     
#     sub <- data_by_day1 %>%
#       filter(per==i) %>%
#       select(all_of(j))
#     
#     colnames(sub) <- "vals"
#     
#     abv_8 <- sub %>%
#       filter(vals>log10(8))
#     
#     abv_8 <- length(abv_8$vals)/length(sub$vals)
#     new_df <- data.frame(per=i, iter=j, prop=abv_8)
#     
#     ci_thresh <- ci_thresh %>%
#       bind_rows(new_df)
#   }
# }
# 
# ci_thresh_summary <- ci_thresh %>%
#   group_by(per) %>%
#   summarise(prop_mean = mean(prop),
#             prop_2.5 = quantile(prop, .025),
#             prop_97.5 = quantile(prop, .975))