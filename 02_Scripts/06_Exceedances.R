library(tidyverse)
library(data.table)
library(readxl)
library(foreach)
library(doParallel)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(!str_detect(wq_names, "Temperature|Organic nitrogen|Chloride"))
unique(wq_names$wq_names)

max_min <- read.csv("03_Results/max_min.csv")
#max_min <- read.csv("/blue/carpena/caseyharris/Ch2/max_min.csv")
flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
#flow_Alaf_Hills <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Alaf_Hills.csv")
flow_jason <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
#flow_jason <- read.csv("/blue/carpena/caseyharris/Ch2/flow_Jason1.csv")

cluster <- makeCluster(6)
registerDoParallel(cluster)

i=2
foreach(i = 1:length(wq_names$wq_names),
        .packages=c('tidyverse', 'data.table', 'readxl')) %dopar% {
          
  title_use <- gsub(".csv", "", wq_names$wq_names[i])
          
  y_all <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  #y_all <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), header=TRUE)
  
  #discard duplicate Q_cfs_log_round
  y_dups <- y_all %>%
    select(split, row, Q_cfs_log_round)
  y_all$dups <- duplicated(y_dups)
  y_all1 <- y_all %>%
    filter(dups==FALSE)
  
  y_all_wide <- y_all1 %>%
    mutate(row = paste0("r", row, sep="")) %>%
    select(row, Q_cfs_log_round, y) %>%
    pivot_wider(names_from=row, values_from=y) %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3))
  
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
    
      unique(y_all_wide$Q_cfs_log_round)
      unique(flow_past$Q_cfs_log_round)
          
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
      wq_flow_past1 <- wq_flow_past %>%
            mutate(date=as.Date(Date),
                   Chang_item=0,
                   Realiz_num=0,
                   scenario="USGS") %>%
            select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_past))

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
            select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_jason))

            both <- wq_flow_jason1 %>%
              bind_rows(wq_flow_past1)

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
          
            min_wq <- both1 %>%
              select(-title, -scenario, -date, -Chang_item, -Realiz_num, -Q_cfs_log_round, -Q_cfs_log_round_edit) %>%
              summarise(across(everything(), min))
            min_wq <- min_wq %>%
              rowwise() %>%
              mutate(min = min(c_across(7:ncol(min_wq)))) %>%
              select(min)
            min_wq_log <- log(min_wq$min)

            max_wq <- both1 %>%
              select(-title, -scenario, -date, -Chang_item, -Realiz_num, -Q_cfs_log_round, -Q_cfs_log_round_edit) %>%
              summarise(across(everything(), max))
            max_wq <- max_wq %>%
              rowwise() %>%
              mutate(max = max(c_across(7:ncol(max_wq)))) %>%
              select(max)
            max_wq_log <- log(max_wq$max)
            
            wq_list <- seq(from=min_wq_log, to=max_wq_log, length.out=100) #modeled range, may include too large of extremes?
            #append threshold list to wq_list
            
              exceed_df <- data.frame()
              j=1
              k=1
              q=median(wq_list)
              for (j in 1:length(unique(both1$Chang_item))) {
                
                sub <- both1 %>%
                  filter(Chang_item==unique(both1$Chang_item)[j])
                
                for (k in 1:length(unique(y_all$row))) {
                  for (q in wq_list) {
                    
                    sub1 <- data.frame(y_est = sub[,k+13])
                    sub2 <- sub1 %>%
                      mutate(above_thresh = case_when(y_est>exp(q) ~ 1,
                                                      TRUE ~ 0))

                    above <- sum(sub2$above_thresh)
                    total <- length(sub2$above_thresh)
                    prop_exceed_q = round(above/total, 5)

                    new_exceed_df <- data.frame("Chang_item"=j, "row"=k,
                                                "wq_level"=exp(q), "prop_exceed"=prop_exceed_q)

                    exceed_df <- exceed_df %>%
                                  bind_rows(new_exceed_df)
                  }
                }
                print(j)
              } #long time

            j=100
            q=median(wq_list)
            exceed_df1 <- data.frame()
            for (j in unique(exceed_df$Chang_item)) {
              
              sub3 <- exceed_df %>%
                filter(Chang_item==j)
              
              for (q in wq_list) {

                sub4 <- sub3 %>%
                  filter(wq_level==exp(q)) #should be 1000 items

                mean_exceed <- mean(sub4$prop_exceed)
                median_exceed <- median(sub4$prop_exceed)
                low_exceed_90 <- quantile(sub4$prop_exceed, .05)
                high_exceed_90 <- quantile(sub4$prop_exceed, .95)
                low_exceed_95 <- quantile(sub4$prop_exceed, .025) #1000 vals enough for 95%?
                high_exceed_95 <- quantile(sub4$prop_exceed, .975)

                new_exceed_df <- data.frame("Chang_item"=j,
                                            "wq_level"=exp(q), 
                                            "mean_exceed"=mean_exceed,
                                            "median_exceed"=median_exceed,
                                            "low_exceed_90"=low_exceed_90, "high_exceed_90"=high_exceed_90,
                                            "low_exceed_95"=low_exceed_95, "high_exceed_95"=high_exceed_95)

                exceed_df1 <- exceed_df1 %>%
                  bind_rows(new_exceed_df)
                
              }
              print(j)
            }

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

            exceed_df2 <- exceed_df1 %>%
              left_join(model_names2)

            #write.csv(exceed_df2, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance probabilities/", title_use, ".csv", sep=""))
            write.csv(exceed_df2, paste("/blue/carpena/caseyharris/Ch2/Results/Exceedance/", title_use, ".csv", sep=""))

}

  #           # threshold <- dist_df1 %>%
  #           #   filter(y>round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) & y<(round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) + .1)) %>%
  #           #   group_by(Chang_item, model, time) %>%
  #           #   summarise(thresh=mean(per_exceed))
  #           
  #           # ggplot(exceed_df2, aes(wq_level, mean_exceed*100, group=Chang_item, color=model)) +
  #           #   ##geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
  #           #   #geom_hline(yintercept=median(dist_df1$y[dist_df1$Chang_item==0]), linetype="solid", linewidth=1) +
  #           #   ##geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
  #           #   #geom_segment(data=filter(threshold, Chang_item!=4), aes(x=thresh*100, y=-Inf, xend=thresh*100, yend=median(dist_df1$y[dist_df1$Chang_item==0]), group=model, color=model), linetype="dashed", linewidth=1) +
  #           #   geom_line(linewidth=1) +
  #           #   geom_errorbar(aes(ymin=low_exceed_95*100, ymax=high_exceed_95*100, group=Chang_item, color=model)) +
  #           #   geom_errorbar(aes(ymin=low_exceed_90*100, ymax=high_exceed_90*100, group=Chang_item, color=model)) +
  #           #   ##scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
  #           #   #scale_x_continuous(breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
  #           #   ##scale_y_continuous(limits=c(.1, 250)) + #specific to analyte
  #           #   #scale_color_manual(values=c("USGS"="black", "NLDAS-2_Hargreaves"="#7570b3", "GFDL_CM3_Hargreaves"="#1b9e77", "GFDL_ESM2G_Hargreaves"="#d95f02")) +
  #           #   xlab("Water quality (units)") +
  #           #   ylab("Percent exceedance (%)") +
  #           #   facet_wrap(~per, nrow=1) +
  #           #   theme_minimal() +
  #           #   theme(plot.background=element_rect(fill="white"),
  #           #         axis.text=element_text(size=10),
  #           #         axis.title=element_text(size=10),
  #           #         strip.text=element_text(size=10),
  #           #         legend.text=element_text(size=10),
  #           #         legend.title=element_blank(),
  #           #         legend.position="none")
  #           # ggsave(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Exceedance probabilities/", title_use, ".png", sep=""), width=6.5, height=3.5)
  #           
  #         }