library(tidyverse)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
wq_names_df <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris"))
unique(wq_names_df$wq_names)

max_min <- read.csv("03_Results/max_min.csv")
flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
flow_jason <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")

i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names_df$wq_names) {
  
  title_use <- gsub(".csv", "", i)
  # wq <- read.csv(paste0("01_Data/Orig wq and flow/WQ/", i, sep="")) #water quality data
  # wq <- wq %>%
  #   mutate(Date = as.Date(Date),
  #          wq_orig = ConcAve) %>%
  #   select(Date, wq_orig)
  
  mcmc_summary <- read.csv(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC JAGS/", title_use, ".csv", sep=""))
  cal_val <- read.csv(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val JAGS/", title_use, ".csv", sep=""))
  n_obs <- cal_val %>%
    filter(split==1)
  n_obs <- length(n_obs$title)
  
  #y vals
  j=2
  k=3
  y_all <- data.frame()
  cal_val1 <- cal_val
  #for (j in 1:10) {
  for (j in 1:length(mcmc_summary$title)) {
    
    y_row <- data.frame()
    
    for (k in 1:n_obs) {
      
      Q_cfs_log_round <- cal_val$Q_cfs_log_round[k]
      split <- mcmc_summary$split[j]
      set.seed(j*k)
      y <- rgamma(1, mcmc_summary$r[j], mcmc_summary$r[j]/mcmc_summary[j,k+8])
      
      y_new <- data.frame("title"=title_use, "split"=split, "row"=j, "Q_cfs_log_round"=Q_cfs_log_round, "y"=y)
      y_row <- y_row %>%
        bind_rows(y_new)
      y_all <- y_all %>%
        bind_rows(y_new)

    }
    
    cal_val1[,j+5] <- y_row$y
    print(j)
  }
  
  #discard duplicate Q_cfs_log_round
  y_dups <- y_all %>%
    select(title, split, row, Q_cfs_log_round)
  y_all$dups <- duplicated(y_dups)
  y_all1 <- y_all %>%
    filter(dups==FALSE)
  
  y_all_wide <- y_all1 %>%
    mutate(split_row = paste0("s", split, "r", row, sep="")) %>%
    select(title, split_row, Q_cfs_log_round, y) %>%
    pivot_wider(names_from=split_row, values_from=y)
  
  max_min1 <- max_min %>%
    filter(title==title_use)
  
  if (str_detect(title_use, "Alafia")) {
    flow_past <- flow_Alaf_Hills %>%
      filter(site=="Alafia") %>%
      select(Date, Q_cfs_log_round)
    flow_jason1 <- flow_jason %>%
      filter(site=="Alafia")
  } else if (str_detect(title_use, "Morris")) {
    flow_past <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough") %>%
      select(Date, Q_cfs_log_round)
    flow_jason1 <- flow_jason %>%
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
    left_join(y_all_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  wq_flow_past1 <- wq_flow_past %>%
    mutate(date=as.Date(Date),
           Chang_item=0,
           Realiz_num=0,
           scenario="USGS") %>%
    select(title, scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, s1r1:s5r1000)
  
  wq_flow_jason <- flow_jason1 %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_wq_2010"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_wq_2010"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_wq_2010"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_wq_2010"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    left_join(y_all_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  wq_flow_jason1 <- wq_flow_jason %>%
    mutate(date=as.Date(date)) %>%
    select(title, scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, s1r1:s5r1000)
  
  both <- wq_flow_jason1 %>%
    bind_rows(wq_flow_past1)
  
  both_usgs1 <- both %>%
    filter(scenario=="USGS" & date>=as.Date("1989-01-01") & date<as.Date("2013-01-01"))
  
  both_usgs2 <- both %>%
    filter(scenario=="USGS" & date>=as.Date("2010-01-01") & date<as.Date("2020-01-01")) %>%
    mutate(Chang_item = 1369,
           Realiz_num = 77)
  
  both1 <- both %>%
    filter(scenario!="IHM_original" & scenario!="USGS") %>%
    bind_rows(both_usgs1) %>%
    bind_rows(both_usgs2)
  
  wq_list <- seq(from=max_min1$value[max_min1$name=="min_wq_2010"], to=max_min1$value[max_min1$name=="max_wq_2010"], length.out=100)
  l=100
  m=8
  exceed_df <- data.frame()
  for (l in unique(both1$Chang_item)) {
    for (m in 8:1007) {
      
      sub <- both1 %>%
        filter(Chang_item==l)
      
      for (q in wq_list) {
        
        sub1 <- sub %>%
          mutate(above_thresh = case_when(sub[,m]>q ~ 1,
                                          TRUE ~ 0))#%>%
        
        above <- sum(sub1$above_thresh)
        total <- length(sub1$above_thresh)
        prop_exceed_q = above/total
        
        new_exceed_df <- data.frame("title"=title_use, "Chang_item"=l, "jags_row"=m,
                                    "wq_level"=q, "prop_exceed"=prop_exceed_q)
        
        exceed_df <- exceed_df %>%
          bind_rows(new_exceed_df)
      }
    }
    print(l)
  }
  
  l=100
  q=wq_list[50]
  exceed_df1 <- data.frame()
  for (l in unique(exceed_df$Chang_item)) {
    for (q in wq_list) {
    
    sub <- exceed_df %>%
      filter(wq_level==q)
    
    mean_exceed <- mean(sub$prop_exceed)
    median_exceed <- median(sub$prop_exceed)
    low_exceed_90 <- quantile(sub$prop_exceed, .05)
    high_exceed_90 <- quantile(sub$prop_exceed, .95)
    low_exceed_95 <- quantile(sub$prop_exceed, .025) #1000 vals enough for 95%?
    high_exceed_95 <- quantile(sub$prop_exceed, .975)
    
    new_exceed_df <- data.frame("title"=title_use, "Chang_item"=l,
                                "wq_level"=q, "mean_exceed"=mean_exceed,
                                "median_exceed"=median_exceed,
                                "low_exceed_90"=low_exceed_90, "high_exceed_90"=high_exceed_90,
                                "low_exceed_95"=low_exceed_95, "high_exceed_95"=high_exceed_95)
    
    exceed_df1 <- exceed_df1 %>%
      bind_rows(new_exceed_df)
    }
    print(l)
  }
  
  library(readxl)
  model_names <- read_excel("C:/Users/cshar/Desktop/Ch2_git/Ch2/01_Data/Vgrids_realization_list.xlsx", sheet="Numbering", col_names=c("Realiz_num", "Scenario", 1:18), skip=2)
  colnames(model_names) <- make.names(colnames(model_names))
  model_names1 <- model_names %>%
    mutate(Chang_item = X4,
           model = Scenario) %>%
    select(model, Chang_item) %>%
    mutate(model = str_remove(model, "_rcp85")) %>%
    mutate(model = str_remove_all(model, "'"))
  
  add_names <- data.frame("model"=c("USGS", "USGS"), "Chang_item"=c(0,1369))
  model_names1 <- model_names1 %>%
    bind_rows(add_names)
  unique(model_names1$model)
  
  model_names2 <- model_names1 %>%
    mutate(per = case_when(Chang_item<=126 & Chang_item!=4 ~ "1989-2012",
                           Chang_item>=649 & Chang_item<=672 ~ "2030-2060",
                           Chang_item>=673 & Chang_item!=1369 ~ "2070-2100",
                           Chang_item==4 ~ "1989-2005",
                           Chang_item==1369 ~ "2010-2019",
                           TRUE ~ NA_character_))
  
  exceed_df2 <- exceed_df1 %>%
    left_join(model_names2)
 
  # threshold <- dist_df1 %>%
  #   filter(y>round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) & y<(round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) + .1)) %>%
  #   group_by(Chang_item, model, time) %>%
  #   summarise(thresh=mean(per_exceed))
 
  ggplot(exceed_df2, aes(wq_level, mean_exceed*100, group=Chang_item, color=model)) +
    ##geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
    #geom_hline(yintercept=median(dist_df1$y[dist_df1$Chang_item==0]), linetype="solid", linewidth=1) +
    ##geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
    #geom_segment(data=filter(threshold, Chang_item!=4), aes(x=thresh*100, y=-Inf, xend=thresh*100, yend=median(dist_df1$y[dist_df1$Chang_item==0]), group=model, color=model), linetype="dashed", linewidth=1) +
    geom_line(linewidth=1) +
    geom_errorbar(aes(ymin=low_exceed_95*100, ymax=high_exceed_95*100, group=Chang_item, color=model)) +
    geom_errorbar(aes(ymin=low_exceed_90*100, ymax=high_exceed_90*100, group=Chang_item, color=model)) +
    ##scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    #scale_x_continuous(breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    ##scale_y_continuous(limits=c(.1, 250)) + #specific to analyte
    #scale_color_manual(values=c("USGS"="black", "NLDAS-2_Hargreaves"="#7570b3", "GFDL_CM3_Hargreaves"="#1b9e77", "GFDL_ESM2G_Hargreaves"="#d95f02")) +
    xlab("Water quality (units)") +
    ylab("Percent exceedance (%)") +
    facet_wrap(~per, nrow=1) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          legend.position="none")
  ggsave(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Exceedance probabilities/", title_use, ".png", sep=""), width=6.5, height=3.5)

  ggplot(filter(dist_df1, Chang_item!=4), aes(y, per_exceed*100, group=model, color=model)) +
    #geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
    geom_vline(xintercept=median(dist_df1$y[dist_df1$Chang_item==0]), linetype="solid", linewidth=1) +
    #geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
    geom_segment(data=filter(threshold, Chang_item!=4), aes(y=thresh*100, x=-Inf, yend=thresh*100, xend=median(dist_df1$y[dist_df1$Chang_item==0]), group=model, color=model), linetype="dashed", linewidth=1) +
    geom_line(linewidth=1) +
    #scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    scale_y_continuous(breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    #scale_y_continuous(limits=c(.1, 250)) + #specific to analyte
    scale_color_manual(values=c("USGS"="black", "NLDAS-2_Hargreaves"="#7570b3", "GFDL_CM3_Hargreaves"="#1b9e77", "GFDL_ESM2G_Hargreaves"="#d95f02")) +
    xlab("Water quality (units)") +
    ylab("Percent exceedance (%)") +
    facet_wrap(~time, nrow=1) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          legend.position="none")
  ggsave(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Exceedance probabilities/", title_use, " trans.png", sep=""), width=6.5, height=3.5)
  
}

#What about CI?
ci_thresh <- data.frame()
j=0
for (j in unique(dist_df1$Chang_item)) {
  
    sub_ci <- dist_df1 %>%
      filter(Chang_item==j)

    above_thresh <- sub_ci %>%
      filter(y>2.8)
    
    prop_abv <- length(above_thresh$y)/length(sub_ci$y)
    new_df <- data.frame(Chang_item=j, prop=prop_abv)

    ci_thresh <- ci_thresh %>%
      bind_rows(new_df)
    
}

#HOW TO CALCULATE CI

ci_thresh_summary <- ci_thresh %>%
  group_by(per) %>%
  summarise(prop_mean = mean(prop),
            prop_2.5 = quantile(prop, .025),
            prop_97.5 = quantile(prop, .975))