#This script needs more work
#calculates water quality exceedances
#needs thresholds of interest

library(tidyverse)
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

model_names2 <- model_names2 %>%
  filter(str_detect(model, "Hargreaves") | model=="USGS")

rm(add_names)
rm(model_names)
rm(model_names1)

#stopCluster(cluster)
#cluster <- makeCluster(8)
#registerDoParallel(cluster)
i=1
foreach(i = 1:length(wq_names$wq_names),
        .packages=c('tidyverse', 'readxl')) %dopar% {
          
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
  
  y_wide <- y_all1 %>%
    mutate(row = paste0("s", row, sep="")) %>%
    select(row, Q_cfs_log_round, y) %>%
    pivot_wider(names_from=row, values_from=y) %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3))
  
  mu_wide <- y_all1 %>%
    mutate(row = paste0("s", row, sep="")) %>%
    select(row, Q_cfs_log_round, y_exp_alt) %>%
    pivot_wider(names_from=row, values_from=y_exp_alt) %>%
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
  
  #For the y
  wq_flow_past <- flow_past %>%
            mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3)) %>%
            mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                   min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"]) %>%
            mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                                      TRUE ~ "out")) %>%
            mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                                                    in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"],
                                                    in_out=="in" ~ Q_cfs_log_round)) %>%
            left_join(y_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))

  wq_flow_past1 <- wq_flow_past %>%
            mutate(date=as.Date(Date),
                   Chang_item=0,
                   Realiz_num=0,
                   scenario="USGS") %>%
            select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_past))
  rm(wq_flow_past)
  gc()
  
  wq_flow_jason <- flow_jason1 %>%
            mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
            mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                   min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"]) %>%
            mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                                      TRUE ~ "out")) %>%
            mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                                                    in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"],
                                                    in_out=="in" ~ Q_cfs_log_round)) %>%
            left_join(y_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  rm(y_wide)
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

  both_y <- both %>%
              filter(scenario!="USGS") %>%
              bind_rows(both_usgs1) %>%
              bind_rows(both_usgs2)
  rm(both_usgs1)
  rm(both_usgs2)
  rm(both)
  gc()
  
  #For the mu
  wq_flow_past <- flow_past %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    left_join(mu_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  rm(flow_past)
  gc()
  
  wq_flow_past1 <- wq_flow_past %>%
    mutate(date=as.Date(Date),
           Chang_item=0,
           Realiz_num=0,
           scenario="USGS") %>%
    select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_past))
  rm(wq_flow_past)
  
  wq_flow_jason <- flow_jason1 %>%
    mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
    mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
           min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"]) %>%
    mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                              TRUE ~ "out")) %>%
    mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2010_2019"],
                                            in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2010_2019"],
                                            in_out=="in" ~ Q_cfs_log_round)) %>%
    left_join(mu_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
  rm(flow_jason1)
  rm(mu_wide)
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
  
  both_mu <- both %>%
    filter(scenario!="USGS") %>%
    bind_rows(both_usgs1) %>%
    bind_rows(both_usgs2)
  
  rm(both_usgs1)
  rm(both_usgs2)
  rm(both)
  gc()
  

  #SUMMARIZE
  both_mu <- both_mu %>%
    mutate(month = month(date, label=TRUE, abbr=TRUE))
  both_y <- both_y %>%
    mutate(month = month(date, label=TRUE, abbr=TRUE))

      sub_mu <- both_mu %>%
        # filter(Chang_item==1369 | 
        #           Chang_item==107 |
        #           Chang_item==110 |
        #           Chang_item==653 |
        #           Chang_item==656 |
        #           Chang_item==677 |
        #           Chang_item==680) %>%
        pivot_longer(cols=s1:s1000, names_to="sim", values_to="val") %>%
        mutate(type = "Expected values based on C-Q")
      rm(both_mu)
      gc()
      
      sub_y <- both_y %>%
        # filter(Chang_item==1369 | 
        #          Chang_item==107 |
        #          Chang_item==110 |
        #          Chang_item==653 |
        #          Chang_item==656 |
        #          Chang_item==677 |
        #          Chang_item==680) %>%
        pivot_longer(cols=s1:s1000, names_to="sim", values_to="val") %>%
        mutate(type = "Values with sampling variability")
      rm(both_y)
      gc()
      
      sub_both <- sub_mu %>%
        bind_rows(sub_y) %>%
        left_join(model_names2)
      rm(sub_mu)
      rm(sub_y)
      gc()
      
      # sub_sub <- sub_both %>%
      #   filter(month==6) %>%
      #   filter(model!="USGS")
      
      #BOXPLOTS
      #Want to make these plots for Wendy, need to make sure they work first
      # ggplot(sub_both, aes(per, val, color=type)) +
      #   geom_boxplot() +
      #   facet_wrap(month~model, scales="free_x") +
      #   ggtitle(title_use) +
      #   ylab("") +
      #   xlab("")
      # ggsave(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/mu vs y/", title_use, ".png"), width=9, height=20)
      
      #TILE PLOTS
      m="GFDL_CM3_Hargreaves"
      n="1989-2012"
      o=6 #change to o="Jun"
      p="s1"
      sub_both1 <- sub_both %>%
        filter(type=="y") %>%
        filter(model=="GFDL_CM3_Hargreaves" | model=="GFDL_ESM2G_Hargreaves") #%>% remove this line to do all models
        # filter(sim=="s1" | sim=="s2" | sim=="s3" | sim=="s4" | sim=="s5" |
        #          sim=="s6" | sim=="s7" | sim=="s8" | sim=="s9" | sim=="s10")

      sel_vals <- data.frame()
      for (m in unique(sub_both1$model)) {
        sub_sel <- sub_both1 %>%
          filter(model==m)
        for (n in unique(sub_sel$per)) {
          sub_sel1 <- sub_sel %>%
            filter(per==n)
          for (o in unique(sub_sel1$month)) {
            sub_sel2 <- sub_sel1 %>%
              filter(month==o)
            for (p in unique(sub_sel2$sim)) {
              sub_sel3 <- sub_sel2 %>%
                filter(sim==p)

              sel_vals_new <- data.frame("model"=m, "per"=n, "month"=o, "sim"=p, "iter"=c(1:1000), "val"=sample(sub_sel3$val, 1000, replace=TRUE)) #can't remember if all years equal or if temporal trend?
              sel_vals <- sel_vals %>%
                bind_rows(sel_vals_new)
              print(p)
            }
           print(o)
          }
          print(n)
        }
        print(m)
      }
          
      sel_vals_wide <- sel_vals %>%
        pivot_wider(id_cols=c(model, month, sim, iter), names_from=per, values_from=val)
      
      colnames(sel_vals_wide) = make.names(colnames(sel_vals_wide))

      sel_vals_wide1 <- sel_vals_wide %>%
        mutate(fut1_past = X2030.2060 - X1989.2012,
               fut2_past = X2070.2100 - X1989.2012)
      
      sel_vals_summary <- sel_vals_wide1 %>%
        group_by(model, month, sim) %>%
        summarise(sim_median_past = median(X1989.2012),
                  sim_median_diff1 = median(fut1_past),
                  sim_median_diff2 = median(fut2_past)) %>%
        ungroup() %>%
        mutate(pos_neg1 = case_when(sim_median_diff1>0 ~ "pos",
                                    sim_median_diff1<=0 ~ "neg"),
               pos_neg2 = case_when(sim_median_diff2>0 ~ "pos",
                                    sim_median_diff2<=0 ~ "neg")) %>%
        group_by(model, month) %>%
        summarise(pos_diff1 = sum(pos_neg1=="pos"),
                  neg_diff1 = sum(pos_neg1=="neg"),
                  pos_diff2 = sum(pos_neg2=="pos"),
                  neg_diff2 = sum(pos_neg2=="neg"),
                  median_past = median(sim_median_past),
                  median_diff1 = median(sim_median_diff1),
                  median_diff2 = median(sim_median_diff2),
                  ci_low1 = quantile(sim_median_diff1, .05),
                  ci_high1 = quantile(sim_median_diff1, .95),
                  ci_low2 = quantile(sim_median_diff2, .05),
                  ci_high2 = quantile(sim_median_diff2, .95)) %>%
        ungroup()
      
      sel_vals_summary1 <- sel_vals_summary #%>%
        # mutate(month_names = case_when(month==1 ~ "Jan",
        #                                month==2 ~ "Feb",
        #                                month==3 ~ "Mar",
        #                                month==4 ~ "Apr",
        #                                month==5 ~ "May",
        #                                month==6 ~ "Jun",
        #                                month==7 ~ "Jul",
        #                                month==8 ~ "Aug",
        #                                month==9 ~ "Sep",
        #                                month==10 ~ "Oct",
        #                                month==11 ~ "Nov",
        #                                month==12 ~ "Dec")) %>%
        # mutate(month_names = ordered(as.factor(month_names), levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) %>%
        # mutate(month_names = fct_rev(month_names))
      
      ggplot(sel_vals_summary1, aes(model, month_names, fill=median_diff1)) +
        geom_tile() +
        scale_fill_gradient2() +
        ggtitle(title_use, subtitle="Difference between future and retrospective medians") +
        labs(fill='Change (mg/l)') +
        theme_minimal() +
        theme(axis.title=element_blank())
      }
  
  min_wq <- both1 %>%
              select((ncol(both1)-999):ncol(both1)) %>%
              summarise(across(everything(), min)) %>%
              rowwise() %>%
              mutate(min = min(c_across(1:1000))) %>%
              select(min)
  min_wq_log <- log(min_wq$min)

  max_wq <- both1 %>%
              select((ncol(both1)-999):ncol(both1)) %>%
              summarise(across(everything(), max)) %>%
              mutate(max = max(c_across(1:1000))) %>%
              select(max)
  max_wq_log <- log(max_wq$max)
  
  #median of past wq
  median_wq <- both1 %>%
    filter(Chang_item==1369) %>%
    select((ncol(both1)-999):ncol(both1)) %>%
    summarise(across(everything(), median)) %>%
    mutate(median = median(c_across(1:1000))) %>%
    select(median)
  median_wq_log <- log(median_wq$median)
  
  #eventually make another list
  #choose thresholds, then calculate 2010-2020 flow percentiles
  #then % of wq values exceeding chosen threshold at each flow percentile
  
  wq_list <- seq(from=min_wq_log, to=max_wq_log, length.out=100) #modeled range, may include too large of extremes?
  wq_list <- c(wq_list, median_wq_log)
  #append other desired thresholds list to wq_list
            
  #For each Chang model (78 models),
  #subset one model at a time for the wq parameter of interest,
  #then select one set of JAGS estimates at a time (1000 realizations), 
  #and estimate exceedances for each wq level in the wq_list
  
              j=4
              k=1
              q=wq_list[50]
              for (j in unique(models_to_use$Chang_item)) {
                
                exceed_df <- data.frame()
                
                sub <- both1 %>%
                  filter(Chang_item==j)
               
                for (k in 1:(ncol(sub)-13)) {
                  for (q in wq_list) {
                    
                    sub1 <- data.frame(y_est = sub[,k+13])
                    sub2 <- sub1 %>%
                      mutate(above_thresh = case_when(y_est>exp(q) ~ 1,
                                                      TRUE ~ 0))

                    above <- sum(sub2$above_thresh)
                    total <- length(sub2$above_thresh)
                    prop_exceed_q = round(above/total, 4)

                    new_exceed_df <- data.frame("Chang_item"=j, "row"=k,
                                                "wq_level"=exp(q), "prop_exceed"=prop_exceed_q)

                    exceed_df <- exceed_df %>%
                                  bind_rows(new_exceed_df)
                    
                    rm(sub1)
                    rm(sub2)
                    rm(above)
                    rm(total)
                    rm(prop_exceed_q)
                  }
                  print(j)
                  print(k)
                }
                
                if (j==1) {
                  write.table(exceed_df, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=TRUE)
                  #write.table(exceed_df, paste("/blue/carpena/caseyharris/Ch2/Results/Exceedance in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=TRUE)
                } else {
                  write.table(exceed_df, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
                  #write.table(exceed_df, paste("/blue/carpena/caseyharris/Ch2/Results/Exceedance in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
                }
                
                rm(sub)
                rm(exceed_df)
              } #around 20 hrs per wq parameter

            exceed_df <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance in prog/", title_use, ".csv", sep=""), header=TRUE)
            #exceed_df <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/Exceedance in prog/", title_use, ".csv", sep=""), header=TRUE)
            
            j=4
            q=wq_list[50]
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
                                            "median_wq_val"=median_wq_log,
                                            "mean_exceed"=mean_exceed,
                                            "median_exceed"=median_exceed,
                                            "low_exceed_90"=low_exceed_90, 
                                            "high_exceed_90"=high_exceed_90,
                                            "low_exceed_95"=low_exceed_95, 
                                            "high_exceed_95"=high_exceed_95)

                exceed_df1 <- exceed_df1 %>%
                  bind_rows(new_exceed_df)
                
                rm(sub4)
                rm(mean_exceed)
                rm(median_exceed)
                rm(low_exceed_90)
                rm(high_exceed_90)
                rm(low_exceed_95)
                rm(high_exceed_95)
                
              }
              rm(sub3)
            }
            rm(exceed_df)
            gc()

            exceed_df2 <- exceed_df1 %>%
              left_join(model_names2)
            rm(exceed_df1)

            write.csv(exceed_df2, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance/", title_use, ".csv", sep=""))
            #write.csv(exceed_df2, paste("/blue/carpena/caseyharris/Ch2/Results/Exceedance/", title_use, ".csv", sep=""))
            
            rm(new_exceed_df)
            rm(exceed_df2)
            rm(max_wq)
            rm(min_wq)
            rm(both1)
            rm(title_use)
            rm(wq_list)
            gc()

}
stopCluster(cluster)

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