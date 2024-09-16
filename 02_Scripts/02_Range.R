library(tidyverse)

flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
wq_names_df <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris"))
unique(wq_names_df$wq_names)

max_min <- data.frame()
i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names_df$wq_names) {
  
  title <- gsub(".csv", "", i)
  
  wq <- read.csv(paste0("01_Data/Orig wq and flow/WQ/", i, sep="")) #water quality data
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen)
  
  if (str_detect(title, "Alafia")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Alafia") %>%
      select(Date, Q_cfs_log_round)
    site <- "Alafia"
  } else if (str_detect(title, "Morris")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough") %>%
      select(Date, Q_cfs_log_round)
    site <- "Hillsborough"
  }
  
  wq_flow <- flow0 %>%
    mutate(Date = as.Date(Date)) %>%
    left_join(wq0, by="Date")
  
  max_flow_ever <- max(wq_flow$Q_cfs)
  min_flow_ever <- min(wq_flow$Q_cfs)
  max_flow_2010 <- max(wq_flow$Q_cfs[wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])
  min_flow_2010 <- min(wq_flow$Q_cfs[wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])
  
  max_wq_ever <- max(wq_flow$ConcAve[!is.na(wq_flow$ConcAve)])
  min_wq_ever <- min(wq_flow$ConcAve[!is.na(wq_flow$ConcAve)])
  max_wq_2010 <- max(wq_flow$ConcAve[!is.na(wq_flow$ConcAve) & wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])
  min_wq_2010 <- min(wq_flow$ConcAve[!is.na(wq_flow$ConcAve) & wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])
  
  max_flow_wq_ever <- 10^mean(log10(max(wq_flow$Q_cfs[!is.na(wq_flow$ConcAve)])))
  min_flow_wq_ever <- 10^mean(log10(min(wq_flow$Q_cfs[!is.na(wq_flow$ConcAve)])))
  max_flow_wq_2010 <- 10^mean(log10(max(wq_flow$Q_cfs[!is.na(wq_flow$ConcAve) & wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])))
  min_flow_wq_2010 <- 10^mean(log10(min(wq_flow$Q_cfs[!is.na(wq_flow$ConcAve) & wq_flow$Date>=as.Date("2010-01-01") & wq_flow$Date<as.Date("2020-01-01")])))

  max_min_new <- data.frame("title"=title, "site"=site, 
                            "max_flow_ever" = max_flow_ever,
                            "min_flow_ever" = min_flow_ever,
                            "max_flow_2010" = max_flow_2010,
                            "min_flow_2010" = min_flow_2010,
                            "max_wq_ever" = max_wq_ever,
                            "min_wq_ever" = min_wq_ever,
                            "max_wq_2010" = max_wq_2010,
                            "min_wq_2010" = min_wq_2010,
                            "max_flow_wq_ever" = max_flow_wq_ever,
                            "min_flow_wq_ever" = min_flow_wq_ever,
                            "max_flow_wq_2010" = max_flow_wq_2010,
                            "min_flow_wq_2010" = min_flow_wq_2010)
  
  max_min_new1 <- max_min_new %>%
    pivot_longer(cols=3:14)
  
  max_min <- max_min %>%
    bind_rows(max_min_new1)
  
}

write.csv(max_min, "03_Results/max_min.csv", row.names=FALSE)

#full flow range
full_flow_range <- data.frame("Q_cfs_log_round" = seq(from=-8.082, to=10.616, by=.001))
full_flow_range <- full_flow_range %>%
  mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round)))
unique(full_flow_range$Q_cfs_log_round) #18699 too many

#What flow values to develop WQ estimates for?
unique(max_min$name)
max(max_min$value[max_min$name=="max_flow_ever"]) #max flow of the past
min(max_min$value[max_min$name=="min_flow_ever"]) #min flow of the past
max(max_min$value[max_min$name=="max_flow_wq_ever"]) #max flow when wq was ever measured
min(max_min$value[max_min$name=="min_flow_wq_ever"]) #min flow when wq was ever measured
max(max_min$value[max_min$name=="max_flow_wq_2010"]) #max flow when wq was measured since 2010
min(max_min$value[max_min$name=="min_flow_wq_2010"]) #min flow when wq was measured since 2010
#could further pare this down to max/min of individual constituents at each site

unique(flow_Alaf_Hills$Q_cfs_log) #1336
unique(round(flow_Alaf_Hills$Q_cfs_log, 2)) #712
unique(round(flow_Alaf_Hills$Q_cfs_log, 3)) #1336 #use 3 decimal places
unique_Alaf_Hills <- unique(flow_Alaf_Hills$Q_cfs_log_round[flow_Alaf_Hills$Q_cfs_log>=min(max_min$value[max_min$name=="min_flow_wq_2010"]) &
                                   flow_Alaf_Hills$Q_cfs_log<=max(max_min$value[max_min$name=="max_flow_wq_2010"])]) #2083
unique_Jason <- unique(flow_Jason1$Q_cfs_log_round[flow_Jason1$Q_cfs_log>=min(max_min$value[max_min$name=="min_flow_wq_2010"]) &
                                                     flow_Jason1$Q_cfs_log<=max(max_min$value[max_min$name=="max_flow_wq_2010"])]) #6454
unique_both <- c(unique_Alaf_Hills, unique_Jason)
unique_both <- unique(unique_both)
setdiff(unique_Jason, unique_Alaf_Hills)
setdiff(unique_Alaf_Hills, unique_Jason)
min(unique_both)
max(unique_both)

#flow range to use
flow_range_use <- data.frame("Q_cfs_log_round" = seq(from=1.679, to=8.132, by=.001))
flow_range_use <- flow_range_use %>%
  mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round)))
unique(flow_range_use$Q_cfs_log_round) #6454
write.csv(flow_range_use, "01_Data/full_flow_range.csv", row.names=FALSE)

ggplot(data=filter(flow_Jason1, flow_Jason1$Q_cfs_log>=min(max_min$value[max_min$name=="min_flow_wq_2010"]) &
                            flow_Jason1$Q_cfs_log<=max(max_min$value[max_min$name=="max_flow_wq_2010"])), aes(x=exp(Q_cfs_log_round))) +
  geom_histogram()

#using the same list of flows will simplify code but does result in unnecessary calculations
6454/18699
