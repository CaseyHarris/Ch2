library(tidyverse)
library(readxl)
library(foreach)
library(doParallel)

thresh <- data.frame(param = c("Alkalinity", "Color", "Fluoride", "Iron",
                               "Manganese", "Nitrogen", "TOC",
                               "Phosphorus", "Turbidity"),
                     threshold = c(113, 200, .8, 1200, .15, 1.6, 13, 2.8, 20))

wq_names <- list.files("01_Data/Redone wq and flow")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(str_detect(wq_names, "Alkalinity|Color|Fluoride|Iron|Manganese|Nitrogen|TOC|Phosphorus|Turbidity"))
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
                         Chang_item==1369 ~ "2013-2022",
                         TRUE ~ NA_character_))

model_names2 <- model_names2 %>%
  filter(str_detect(model, "Hargreaves") | model=="USGS")

rm(add_names)
rm(model_names)
rm(model_names1)

# stopCluster(cluster)
# cluster <- makeCluster(5)
# registerDoParallel(cluster)
i=6
# foreach(i = 1:length(wq_names$wq_names),
#          .packages=c('tidyverse', 'readxl')) %dopar% {
for (i in 1:length(wq_names$wq_names)) {
           
          title_use <- gsub(".csv", "", wq_names$wq_names[i])
          
          y_all <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), header=FALSE)
          #y_all <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), header=TRUE)
          
          # y_all <- data.frame("split"=y_all$V1, "row"=y_all$V2, "Q_cfs_log_round"=y_all$V3,
          #                     "Q_cfs_log_round_used"=y_all$V4, "y"=y_all$V5, 
          #                     "y_exp_model"=y_all$V6, "y_exp_alt"=y_all$V7)
          
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
          
          # mu_wide <- y_all1 %>%
          #   mutate(row = paste0("s", row, sep="")) %>%
          #   select(row, Q_cfs_log_round, y_exp_alt) %>%
          #   pivot_wider(names_from=row, values_from=y_exp_alt) %>%
          #   mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3))

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
            mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
                   min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"]) %>%
            mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                                      TRUE ~ "out")) %>%
            mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
                                                    in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"],
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
            mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
                   min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"]) %>%
            mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
                                      TRUE ~ "out")) %>%
            mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
                                                    in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"],
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
            filter(scenario=="USGS" & date>=as.Date("2013-01-01") & date<as.Date("2022-01-01")) %>%
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
          # wq_flow_past <- flow_past %>%
          #   mutate(Q_cfs_log_round = round(Q_cfs_log_round, 3)) %>%
          #   mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
          #          min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"]) %>%
          #   mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
          #                             TRUE ~ "out")) %>%
          #   mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
          #                                           in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"],
          #                                           in_out=="in" ~ Q_cfs_log_round)) %>%
          #   left_join(mu_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
          # rm(flow_past)
          # gc()
          # 
          # wq_flow_past1 <- wq_flow_past %>%
          #   mutate(date=as.Date(Date),
          #          Chang_item=0,
          #          Realiz_num=0,
          #          scenario="USGS") %>%
          #   select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_past))
          # rm(wq_flow_past)
          # 
          # wq_flow_jason <- flow_jason1 %>%
          #   mutate(Q_cfs_log_round = round(Q_cfs_log_round, 1)) %>%
          #   mutate(max_flow_wq_2010 = max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
          #          min_flow_wq_2010 = max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"]) %>%
          #   mutate(in_out = case_when(Q_cfs_log_round>=min_flow_wq_2010 & Q_cfs_log_round<=max_flow_wq_2010 ~ "in",
          #                             TRUE ~ "out")) %>%
          #   mutate(Q_cfs_log_round_edit = case_when(in_out=="out" & Q_cfs_log_round<min_flow_wq_2010 ~ max_min1$value[max_min1$name=="max_flow_obs_wq_2013_2022"],
          #                                           in_out=="out" & Q_cfs_log_round>max_flow_wq_2010 ~ max_min1$value[max_min1$name=="min_flow_obs_wq_2013_2022"],
          #                                           in_out=="in" ~ Q_cfs_log_round)) %>%
          #   left_join(mu_wide, by=c("Q_cfs_log_round_edit"="Q_cfs_log_round"))
          # rm(flow_jason1)
          # rm(mu_wide)
          # gc()
          # 
          # wq_flow_jason1 <- wq_flow_jason %>%
          #   mutate(date=as.Date(date)) %>%
          #   select(scenario, date, Chang_item, Realiz_num, Q_cfs_log_round, Q_cfs_log_round_edit, 7:ncol(wq_flow_jason))
          # rm(wq_flow_jason)
          # gc()
          # 
          # both <- wq_flow_jason1 %>%
          #   bind_rows(wq_flow_past1)
          # rm(wq_flow_jason1)
          # rm(wq_flow_past1)
          # gc()
          # 
          # both_usgs1 <- both %>%
          #   filter(scenario=="USGS" & date>=as.Date("1989-01-01") & date<as.Date("2013-01-01"))
          # 
          # both_usgs2 <- both %>%
          #   filter(scenario=="USGS" & date>=as.Date("2010-01-01") & date<as.Date("2020-01-01")) %>%
          #   mutate(Chang_item = 1369,
          #          Realiz_num = 77)
          # 
          # both_mu <- both %>%
          #   filter(scenario!="USGS") %>%
          #   bind_rows(both_usgs1) %>%
          #   bind_rows(both_usgs2)
          # 
          # rm(both_usgs1)
          # rm(both_usgs2)
          # rm(both)
          # gc()
          
          
          #SUMMARIZE
          # both_mu <- both_mu %>%
          #   mutate(month = month(date, label=TRUE, abbr=TRUE))
          both_y <- both_y %>%
            mutate(month = lubridate::month(date, label=TRUE, abbr=TRUE))
          # sub_mu <- both_mu %>%
          #   # filter(Chang_item==1369 | 
          #   #           Chang_item==107 |
          #   #           Chang_item==110 |
          #   #           Chang_item==653 |
          #   #           Chang_item==656 |
          #   #           Chang_item==677 |
          #   #           Chang_item==680) %>%
          #   pivot_longer(cols=s1:s1000, names_to="sim", values_to="val") %>%
          #   mutate(type = "Expected values based on C-Q")
          # rm(both_mu)
          # gc()
          
          both_y <- both_y %>%
            # filter(Chang_item==1369 | 
            #          Chang_item==107 |
            #          Chang_item==110 |
            #          Chang_item==653 |
            #          Chang_item==656 |
            #          Chang_item==677 |
            #          Chang_item==680) %>%
            select(Chang_item, month, in_out, s1:s1000) %>% #change to s1000?
            pivot_longer(cols=s1:s1000, names_to="sim", values_to="val") #%>%
            #mutate(type = "Values with sampling variability")

          both_y <- #sub_mu %>%
            #bind_rows(sub_y) %>%
            both_y %>%
            left_join(model_names2)
          #rm(sub_mu)
          
          #START HERE NAPTIME
          
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
          o="Jun"
          p="s1"
          sub_both <- both_y %>%
            filter(!is.na(model) & model!="USGS" & model!="NLDAS-2_Hargreaves")
          #  filter(type=="y") %>%
          #  filter(model=="GFDL_CM3_Hargreaves" | model=="GFDL_ESM2G_Hargreaves") #%>% remove this line to do all models
          # filter(sim=="s1" | sim=="s2" | sim=="s3" | sim=="s4" | sim=="s5" |
          #          sim=="s6" | sim=="s7" | sim=="s8" | sim=="s9" | sim=="s10")
          sel_vals <- data.frame()
          for (m in unique(sub_both$model)) {
            sub_sel <- sub_both %>%
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
                  
                  sel_vals_new <- data.frame("model"=m, "per"=n, "month"=o, "sim"=p, "iter"=c(1:100), "val"=sample(sub_sel3$val, 100, replace=TRUE)) #can't remember if all years equal or if temporal trend? also, change to 1000?
                  sel_vals <- sel_vals %>%
                    bind_rows(sel_vals_new)
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
          
          # sel_vals_wide1 <- sel_vals_wide %>%
          #   mutate(fut1_past = X2030.2060 - X1989.2012,
          #          fut2_past = X2070.2100 - X1989.2012)
          
          if (str_detect(title_use, "Hillsborough")) {
            param_use <- gsub(" Hillsborough R at Morris Br", "", title_use)
          }
          if (str_detect(title_use, "Alafia")) {
            param_use <- gsub(" Alafia R at Lithia", "", title_use)
          }
          
          thresh_use <- thresh %>%
            filter(param==param_use)
          
          sel_vals_wide1 <- sel_vals_wide %>%
            mutate(thresh_use = thresh_use$threshold[1]) %>%
            mutate(abv_past = case_when(X1989.2012>thresh_use ~ 1,
                                        TRUE ~ 0),
                   abv_fut1 = case_when(X2030.2060>thresh_use ~ 1,
                                        TRUE ~ 0),
                   abv_fut2 = case_when(X2070.2100>thresh_use ~ 1,
                                        TRUE ~ 0))
          
          sel_vals_summary <- sel_vals_wide1 %>%
            group_by(model, month, sim) %>%
            summarise(abv_past_prob = sum(abv_past)/length(abv_past),
                      abv_fut1_prob = sum(abv_fut1)/length(abv_fut1),
                      abv_fut2_prob = sum(abv_fut2)/length(abv_fut2)) %>%
            # summarise(sim_median_past = median(X1989.2012),
            #           sim_median_diff1 = median(fut1_past),
            #           sim_median_diff2 = median(fut2_past)) %>%
            ungroup() %>%
            mutate(diff_fut1_past = abv_fut1_prob - abv_past_prob,
                   diff_fut2_past = abv_fut2_prob - abv_past_prob) %>%
            # mutate(pos_neg1 = case_when(diff_fut1_past>0 ~ "pos",
            #                             diff_fut1_past<=0 ~ "neg"),
            #        pos_neg2 = case_when(diff_fut1_past>0 ~ "pos",
            #                             diff_fut1_past<=0 ~ "neg")) %>%
            group_by(model, month) %>%
            summarise(median_past = median(abv_past_prob),
                      ci_low_past = quantile(abv_past_prob, .05),
                      ci_high_past = quantile(abv_past_prob, .95),
            #          pos_diff1 = sum(pos_neg1=="pos"),
            #          neg_diff1 = sum(pos_neg1=="neg"),
            #          pos_diff2 = sum(pos_neg2=="pos"),
            #          neg_diff2 = sum(pos_neg2=="neg"),
                      median_diff1 = median(diff_fut1_past),
                      median_diff2 = median(diff_fut2_past),
                      ci_low1 = quantile(diff_fut1_past, .05),
                      ci_high1 = quantile(diff_fut1_past, .95),
                      ci_low2 = quantile(diff_fut2_past, .05),
                      ci_high2 = quantile(diff_fut2_past, .95)) %>%
            ungroup()
          
          sel_vals_summary1 <- sel_vals_summary %>%
          mutate(model1 = str_replace(model, "_Hargreaves", "")) %>%
          mutate(model1 = str_replace_all(model1, "_", "-")) %>%
          mutate(model1 = case_when(model1=="bcc-csm" ~ "BCC-CSM",
                                    TRUE ~ model1)) %>%
          mutate(month_names = ordered(as.factor(month), levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))) #%>%
          #mutate(month_names = fct_rev(month_names)) %>%
          # mutate(prob1 = case_when(median_diff1>0 ~ pos_diff1/100,
          #                          median_diff1<0 ~ neg_diff1/100,
          #                          median_diff1==0 ~ NA),
          #        prob2 = case_when(median_diff2>0 ~ pos_diff2/100,
          #                          median_diff2<0 ~ neg_diff2/100,
          #                          median_diff2==0 ~ NA),
          #        prob_label1 = case_when(prob1>=.9 ~ round(prob1, 2),
          #                                prob1>=.8 & prob1<.9 ~ round(prob1, 2)),
          #        prob_label2 = case_when(prob2>=.9 ~ round(prob2, 2),
          #                                prob2>=.8 & prob2<.9 ~ round(prob2, 2)))
          
          # min_diff <- min(c(min(sel_vals_summary1$median_diff1), min(sel_vals_summary1$median_diff2)))
          # max_diff <- max(c(max(sel_vals_summary1$median_diff1), max(sel_vals_summary1$median_diff2)))
          
          # min_diff <- min(c(min(sel_vals_summary1$median_diff1), min(sel_vals_summary1$median_diff2)))
          # max_diff <- max(c(max(sel_vals_summary1$median_diff1), max(sel_vals_summary1$median_diff2)))
          # 
          # ggplot(sel_vals_summary1, aes(month_names, model1, fill=median_diff1)) +
          #   geom_tile() +
          #   #geom_text(aes(label=pos_diff1/100)) +
          #   scale_fill_gradient2(low="blue", high="green", breaks=c(round(min(sel_vals_summary1$median_diff1), 2), 0, round(max(sel_vals_summary1$median_diff1), 2))) +
          #   ggtitle(title_use, subtitle="2030-2060 - retrospective") +
          #   labs(fill='units') +
          #   theme_minimal() +
          #   theme(axis.title=element_blank(), 
          #         legend.position="bottom",
          #         legend.title=element_text(size=10), legend.text=element_text(size=10),
          #         axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
          #         axis.text.y=element_text(size=10),
          #         plot.title=element_text(size=10), plot.subtitle=element_text(size=10))
          # ggsave(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Treshold tileplots/", title_use, " future1.png", sep=""), height=6, width=12)
          # #ggsave(paste("/blue/carpena/caseyharris/Ch2/Results/Treshold tileplots/", title_use, " future1 pos prob.png"), height=6, width=12)
          # #ggsave(paste("/blue/carpena/caseyharris/Ch2/Results/Tileplots/", title_use, " future1.png"), height=3, width=3)
          # 
          # ggplot(sel_vals_summary1, aes(month_names, model1, fill=median_diff2)) +
          #   geom_tile() +
          #   #geom_text(aes(label=pos_diff1/100)) +
          #   scale_fill_gradient2(low="blue", high="green", breaks=c(round(min(sel_vals_summary1$median_diff2), 2), 0, round(max(sel_vals_summary1$median_diff2), 2))) +
          #   ggtitle(title_use, subtitle="2070-2100 - retrospective") +
          #   labs(fill='units') +
          #   theme_minimal() +
          #   theme(axis.title=element_blank(), 
          #         legend.position="bottom",
          #         legend.title=element_text(size=10), legend.text=element_text(size=10),
          #         axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
          #         axis.text.y=element_text(size=10),
          #         plot.title=element_text(size=10), plot.subtitle=element_text(size=10))           
          # ggsave(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Threshold tileplots/", title_use, " future2.png", sep=""), height=6, width=12)
          # #ggsave(paste("/blue/carpena/caseyharris/Ch2/Results/Tileplots/", title_use, " future2.png"), height=3, width=3)
          # 
          sel_vals_summary1 <- sel_vals_summary1 %>%
            mutate(title = title_use)
          
          if (i==1) {
            #write.table(sel_vals_summary1, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Thresholds in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=TRUE)
            write.table(sel_vals_summary1, paste("/blue/carpena/caseyharris/Ch2/Results/Thresholds in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=TRUE)
          } else {
            #write.table(sel_vals_summary1, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Thresholds in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
            write.table(sel_vals_summary1, paste("/blue/carpena/caseyharris/Ch2/Results/Thresholds in prog/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
          }
          
        }
