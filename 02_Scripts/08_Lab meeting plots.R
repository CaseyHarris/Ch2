#Plots for power point

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(!str_detect(wq_names, "Temperature|Organic nitrogen|Chloride"))
unique(wq_names$wq_names)

max_min <- read.csv("03_Results/max_min.csv")

model_names <- read_excel("C:/Users/cshar/Desktop/Ch2_git/Ch2/01_Data/Vgrids_realization_list.xlsx", sheet="Numbering", col_names=c("Realiz_num", "Scenario", 1:18), skip=2)
#model_names <- read_excel("/blue/carpena/caseyharris/Ch2/Vgrids_realization_list.xlsx", sheet="Numbering", col_names=c("Realiz_num", "Scenario", 1:18), skip=2)
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

models_to_use <- model_names2 %>%
  filter(str_detect(model, "Hargreaves") | model=="USGS")

i=7
for (i in 1:length(wq_names$wq_names)) {
  
  title_use <- gsub(".csv", "", wq_names$wq_names[i])
  max_min1 <- max_min %>%
    filter(title==title_use)
  
  y_all <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  #y_all <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  
  cal_val <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title_use, ".csv", sep=""), header=TRUE)
  #cal_val <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/Cal_val/", title_use, ".csv", sep=""), header=TRUE)
  
  y_all1 <- y_all %>%
    mutate(cv_Q = c(rep(cal_val$Q_cfs_log_round[cal_val$split==1], 200), rep(cal_val$Q_cfs_log_round[cal_val$split==2], 200),
                    rep(cal_val$Q_cfs_log_round[cal_val$split==3], 200), rep(cal_val$Q_cfs_log_round[cal_val$split==4], 200),
                    rep(cal_val$Q_cfs_log_round[cal_val$split==5], 200)),
           cv_wq_orig = c(rep(cal_val$wq_orig[cal_val$split==1], 200), rep(cal_val$wq_orig[cal_val$split==2], 200),
                          rep(cal_val$wq_orig[cal_val$split==3], 200), rep(cal_val$wq_orig[cal_val$split==4], 200),
                          rep(cal_val$wq_orig[cal_val$split==5], 200)),
           cv_cal_val = c(rep(cal_val$cal_val[cal_val$split==1], 200), rep(cal_val$cal_val[cal_val$split==2], 200),
                          rep(cal_val$cal_val[cal_val$split==3], 200), rep(cal_val$cal_val[cal_val$split==4], 200),
                          rep(cal_val$cal_val[cal_val$split==5], 200)),
           cv_split = c(rep(cal_val$split[cal_val$split==1], 200), rep(cal_val$split[cal_val$split==2], 200),
                        rep(cal_val$split[cal_val$split==3], 200), rep(cal_val$split[cal_val$split==4], 200),
                        rep(cal_val$split[cal_val$split==5], 200))) %>%
    mutate(check_Q = case_when(Q_cfs_log_round==cv_Q ~ 0,
                               TRUE ~ 1),
           check_split = case_when(split==cv_split ~ 0,
                                   TRUE ~ 1))
  
  sum(y_all1$check_Q)
  sum(y_all1$check_split)
  
  val <- y_all1 %>%
    filter(cv_cal_val=="val")
  val_summary <- val %>%
    group_by(cv_wq_orig) %>%
    summarise(mean = mean(y),
              median = median(y),
              low95 = quantile(y, .025),
              high95 = quantile(y, .975))
  
  cal <- y_all1 %>%
    filter(cv_cal_val=="cal")
  cal_summary <- cal %>%
    group_by(cv_wq_orig) %>%
    summarise(mean = mean(y),
              median = median(y),
              low95 = quantile(y, .025),
              high95 = quantile(y, .975))
  
  check <- val %>%
    group_by(cv_wq_orig) %>%
    summarise(n = n())
  
  check <- cal %>%
    group_by(cv_wq_orig) %>%
    summarise(n = n())
  
  ggplot(val_summary, aes(cv_wq_orig, mean)) +
    geom_abline(aes(intercept=0, slope=1), linetype="dashed") +
    geom_errorbar(aes(ymin=low95, ymax=high95), color="dark gray") +
    geom_point(shape=1) +
    xlab("Observed water quality (units)") +
    ylab("Model-estimated water quality (units)") +
    ggtitle(paste0(title_use, " (validation dataset)\nMean (dots) and 95% range (bars)", sep="")) +
    scale_x_continuous(limits=c(min(c(min(val_summary$low95), min(val_summary$cv_wq_orig), min(cal_summary$cv_wq_orig), min(cal_summary$low95))), max(c(max(val_summary$cv_wq_orig), max(val_summary$high95), max(cal_summary$cv_wq_orig), max(cal_summary$high95))))) +
    scale_y_continuous(limits=c(min(c(min(val_summary$low95), min(val_summary$cv_wq_orig), min(cal_summary$cv_wq_orig), min(cal_summary$low95))), max(c(max(val_summary$cv_wq_orig), max(val_summary$high95), max(cal_summary$cv_wq_orig), max(cal_summary$high95))))) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          plot.title=element_text(size=10),
          legend.text=element_text(size=10),
          legend.position="none")
  ggsave(paste("C:/Users/cshar/Desktop/Ch2_git/Ch2/Lab meeting power point/Obs vs est/", " ", title_use, " val.png", sep=""), width=4.5, height=4.25)
  
  ggplot(cal_summary, aes(cv_wq_orig, mean)) +
    geom_abline(aes(intercept=0, slope=1), linetype="dashed") +
    geom_errorbar(aes(ymin=low95, ymax=high95), color="dark gray") +
    geom_point(shape=1) +
    xlab("Observed water quality (units)") +
    ylab("Model-estimated water quality (units)") +
    ggtitle(paste0(title_use, " (calibration dataset)\nMean (dots) and 95% range (bars)", sep="")) +
    scale_x_continuous(limits=c(min(c(min(val_summary$low95), min(val_summary$cv_wq_orig), min(cal_summary$cv_wq_orig), min(cal_summary$low95))), max(c(max(val_summary$cv_wq_orig), max(val_summary$high95), max(cal_summary$cv_wq_orig), max(cal_summary$high95))))) +
    scale_y_continuous(limits=c(min(c(min(val_summary$low95), min(val_summary$cv_wq_orig), min(cal_summary$cv_wq_orig), min(cal_summary$low95))), max(c(max(val_summary$cv_wq_orig), max(val_summary$high95), max(cal_summary$cv_wq_orig), max(cal_summary$high95))))) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          plot.title=element_text(size=10),
          legend.text=element_text(size=10),
          legend.position="none")
  ggsave(paste("C:/Users/cshar/Desktop/Ch2_git/Ch2/Lab meeting power point/Obs vs est/", " ", title_use, " cal.png", sep=""), width=4.5, height=4.25)
  
  #discard duplicate Q_cfs_log_round
  y_dups <- y_all %>% #don't use y_all1
    select(split, row, Q_cfs_log_round)
  y_all$dups <- duplicated(y_dups)
  #rm(y_dups)
  #gc()
  y_all2 <- y_all %>%
    filter(dups==FALSE)
  #rm(y_all)
  #gc()
  
  y_all2_summary <- y_all2 %>%
    group_by(Q_cfs_log_round) %>%
    summarise(mean = mean(y),
              median = median(y),
              low95 = quantile(y, .025),
              high95 = quantile(y, .975)) %>%
    filter(Q_cfs_log_round>=max_min1$value[max_min1$name=="min_flow_2010"] & Q_cfs_log_round<=max_min1$value[max_min1$name=="max_flow_2010"])
  
  orig <- cal_val %>%
    select(Q_cfs_log_round,
           wq_orig) %>%
    filter(!is.na(wq_orig)) %>%
    unique()
  
  ggplot(y_all2_summary, aes(exp(Q_cfs_log_round), mean)) +
    geom_ribbon(aes(ymin=low95, ymax=high95), fill="dark gray", alpha=0.5) +
    geom_line() +
    geom_point(data=orig, aes(exp(Q_cfs_log_round), wq_orig), shape=1) +
    geom_segment(aes(x=exp(max(y_all2_summary$Q_cfs_log_round)), 
                     xend=exp(max_min1$value[max_min1$name=="max_flow_ever"]), 
                     y=y_all2_summary$mean[y_all2_summary$Q_cfs_log_round==max(y_all2_summary$Q_cfs_log_round)], 
                     yend=y_all2_summary$mean[y_all2_summary$Q_cfs_log_round==max(y_all2_summary$Q_cfs_log_round)]), 
                 linetype="dashed") +
    geom_segment(aes(xend=exp(min(y_all2_summary$Q_cfs_log_round)), 
                     x=exp(max_min1$value[max_min1$name=="min_flow_ever"]), 
                     y=y_all2_summary$mean[y_all2_summary$Q_cfs_log_round==min(y_all2_summary$Q_cfs_log_round)], 
                     yend=y_all2_summary$mean[y_all2_summary$Q_cfs_log_round==min(y_all2_summary$Q_cfs_log_round)]), 
                 linetype="dashed") +
    xlab("Streamflow (cfs)") +
    ylab("Model-estimated water quality (units)") +
    ggtitle(paste0(title_use, "\nObservations (dots), estimates (solid line), estimated 95% range (shaded)\nExtent of future extremes (dashed line)", sep="")) +
    scale_x_log10(limits=c(NA, exp(max_min1$value[max_min1$name=="max_flow_ever"]))) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          plot.title=element_text(size=10),
          legend.text=element_text(size=10),
          legend.position="none")
  ggsave(paste("C:/Users/cshar/Desktop/Ch2_git/Ch2/Lab meeting power point/Obs vs est/", " ", title_use, " flow.png", sep=""), width=4.5, height=4.25)
  
}

library(lubridate)
flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
#flow_Alaf_Hills <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Alaf_Hills.csv")
flow_jason <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
#flow_jason <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Jason1.csv")

i=1
for (i in 1:length(wq_names$wq_names)) {
  
  title_use <- gsub(".csv", "", wq_names$wq_names[i])
  
  y_all <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  #y_all <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  
  #discard duplicate Q_cfs_log_round
  y_dups <- y_all %>%
    select(split, row, Q_cfs_log_round)
  y_all$dups <- duplicated(y_dups)
  rm(y_dups)
  gc()
  y_all1 <- y_all %>%
    filter(dups==FALSE)
  rm(y_all)
  gc()
  
  y_all_wide <- y_all1 %>%
    mutate(row = paste0("r", row, sep="")) %>%
    select(row, Q_cfs_log_round, y) %>%
    pivot_wider(names_from=row, values_from=y) %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3))
  rm(y_all1)
  gc()
  
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
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_wq_2010"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_wq_2010"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_wq_2010"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_wq_2010"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    left_join(y_all_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  rm(flow_past)
  
  wq_flow_past1 <- wq_flow_past %>%
    mutate(date=as.Date(Date),
           Chang_item=0,
           Realiz_num=0,
           scenario="USGS") %>%
    select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_past))
  rm(wq_flow_past)
  
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
  rm(flow_jason1)
  rm(y_all_wide)
  gc()
  
  wq_flow_jason1 <- wq_flow_jason %>%
    mutate(date=as.Date(date)) %>%
    select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_jason))
  rm(wq_flow_jason)
  gc()
  
  both <- wq_flow_jason1 %>%
    bind_rows(wq_flow_past1)
  rm(wq_flow_jason1)
  rm(wq_flow_past1)
  gc()
  
  both_usgs1 <- both %>%
    filter(scenario=="USGS" & date>=as.Date("1989-01-01") & date<as.Date("2013-01-01"))
  
  both_usgs2 <- both %>%
    filter(scenario=="USGS" & date>=as.Date("2010-01-01") & date<as.Date("2020-01-01")) %>%
    mutate(Chang_item = 1369,
           Realiz_num = 77)
  
  both1 <- both %>%
    filter(scenario!="USGS") %>%
    bind_rows(both_usgs1) %>%
    bind_rows(both_usgs2)
  
  rm(both)
  rm(both_usgs1)
  rm(both_usgs2)
  gc()
  
  rm(both1_summarym)
  rm(both1_summaryq)
  gc()
  
  unique(both1$in_out)
  length(both1$Q_cfs[is.na(both1$Q_cfs)])
  both1_Q0 <- both1 %>%
    mutate(month = lubridate::month(date, label=TRUE, abbr=TRUE),
           Q_cfs = case_when(is.na(Q_cfs) ~ exp(Q_cfs_log_round),
                     TRUE ~ Q_cfs)) %>%
    group_by(scenario, Chang_item, month) %>%
    summarise(Q_mean = mean(Q_cfs),
              Q_median = median(Q_cfs),
              Q_.01 = quantile(Q_cfs, .01),
              Q_.025 = quantile(Q_cfs, .025),
              Q_.05 = quantile(Q_cfs, .05),
              Q_.1 = quantile(Q_cfs, .1),
              Q_.25 = quantile(Q_cfs, .25),
              Q_.5 = quantile(Q_cfs, .5),
              Q_.75 = quantile(Q_cfs, .75),
              Q_.9 = quantile(Q_cfs, .9),
              Q_.95 = quantile(Q_cfs, .95),
              Q_.975 = quantile(Q_cfs, .975),
              Q_.99 = quantile(Q_cfs, .99))
  
  both1_Q1 <- both1_Q0 %>%
    ungroup() %>%
    left_join(models_to_use, by="Chang_item") %>%
    filter(!is.na(per)) %>%
    mutate(per = factor(per, ordered=TRUE, levels=c("1989-2012", "2010-2019", "2030-2060", "2070-2100")))
  
  ggplot(both1_Q1, aes(month, Q_mean, group=Chang_item, color=per)) +
    geom_point(position=position_dodge(width=.75)) +
    geom_errorbar(aes(ymin=Q_.01, ymax=Q_.99), width=0, position=position_dodge(width=.75, preserve="single")) +
    scale_y_log10() +
    scale_color_manual(values=c("#bababa", "#404040", "#f4a582", "#ca0020")) +
    theme_minimal()
  
  

              across(r1:r1000, ~ quantile(.x, .5)))
}


          
          
          