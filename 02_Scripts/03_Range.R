# Ranges of water quality observations vs. estimates

#Water quality wasn't measured at all possible flows.
#Water quality observations may be especially sparse 
#at high flows.
#This script creates a table of min/max values of water 
#quality and flow observed in various time periods.
#These max/min values are used in further scripts to 
#know when we are making water quality estimates 
#for flows beyond those with historical water quality 
#observations.

library(tidyverse)

flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")

wq_names <- list.files("01_Data/Redone wq and flow/")
wq_names_df <- data.frame(wq_names) #%>%
  #filter(str_detect(wq_names, "Lithia|Morris"))
unique(wq_names_df$wq_names)

max_min <- data.frame()
i = "Alkalinity Alafia R at Lithia.csv"
for (i in wq_names_df$wq_names) {
  
  title <- gsub(".csv", "", i)
  
  wq <- read.csv(paste0("01_Data/Redone wq and flow/", i, sep="")) #water quality data
  
  wq0 <- wq %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, ConcLow, ConcHigh, ConcAve, Uncen)
  
  if (str_detect(title, "Alafia")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Alafia" & Date<as.Date("2023-01-01")) %>%
      mutate(source="USGS") %>%
      select(source, Date, Q_cfs, Q_cfs_log_round)
    site <- "Alafia"
    flow1 <- flow_Jason1 %>%
      filter(site=="Alafia") %>%
      mutate(source="Chang") %>%
      mutate(Date=date) %>%
      select(source, Date, Q_cfs, Q_cfs_log_round)
  } else if (str_detect(title, "Morris")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough" & Date<as.Date("2023-01-01")) %>%
      mutate(source="USGS") %>%
      select(source, Date, Q_cfs, Q_cfs_log_round)
    site <- "Hillsborough"
    flow1 <- flow_Jason1 %>%
      filter(site=="Hillsborough") %>%
      mutate(source="Chang") %>%
      mutate(Date=date) %>%
      select(source, Date, Q_cfs, Q_cfs_log_round)
  }
  
  wq_flow <- flow0 %>%
    bind_rows(flow1) %>%
    mutate(Date = as.Date(Date)) %>%
    left_join(wq0, by="Date")
  
  #max 1989-2012, 2013-2022, future
  wq_flow1 <- wq_flow %>%
    filter(Date>=as.Date("1989-01-01"))
  max_flow_1989_2100 <- max(wq_flow1$Q_cfs_log_round)
  min_flow_1989_2100 <- min(wq_flow1$Q_cfs_log_round)
  max_flow_1989_2012 <- max(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  min_flow_1989_2012 <- min(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  max_flow_2013_2022 <- max(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])
  min_flow_2013_2022 <- min(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])
  max_flow_future <- max(wq_flow1$Q_cfs_log_round[wq_flow1$Date>=as.Date("2030-01-01")])
  min_flow_future <- min(wq_flow1$Q_cfs_log_round[wq_flow1$Date>=as.Date("2030-01-01")])
  
  #max_wq_1989_2012 <- max(wq_flow1$ConcAve[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  #min_wq_1989_2012 <- min(wq_flow1$ConcAve[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  max_wq_2013_2022 <- max(wq_flow1$ConcAve[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])
  min_wq_2013_2022 <- min(wq_flow1$ConcAve[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])
  
  #max_flow_obs_wq_1989_2012 <- max(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  #min_flow_obs_wq_1989_2012 <- min(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("1989-01-01") & wq_flow1$Date<as.Date("2013-01-01")])
  max_flow_obs_wq_2013_2022 <- max(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])
  min_flow_obs_wq_2013_2022 <- min(wq_flow1$Q_cfs_log_round[wq_flow1$source=="USGS" & !is.na(wq_flow1$ConcAve) & wq_flow1$Date>=as.Date("2013-01-01") & wq_flow1$Date<as.Date("2023-01-01")])

  max_min_new <- data.frame("title"=title, "site"=site, 
                            "max_flow_1989_2100" = max_flow_1989_2100,
                            "min_flow_1989_2100" = min_flow_1989_2100,
                            "max_flow_1989_2012" = max_flow_1989_2012,
                            "min_flow_1989_2012" = min_flow_1989_2012,
                            "max_flow_2013_2022" = max_flow_2013_2022,
                            "min_flow_2013_2022" = min_flow_2013_2022,
                            "max_flow_future" = max_flow_future,
                            "min_flow_future" = min_flow_future,
                            #"max_wq_1989_2012" = max_wq_1989_2012,
                            #"min_wq_1989_2012" = min_wq_1989_2012,
                            "max_wq_2013_2022" = max_wq_2013_2022,
                            "min_wq_2013_2022" = min_wq_2013_2022,
                            #"max_flow_obs_wq_1989_2012" = max_flow_obs_wq_1989_2012,
                            #"min_flow_obs_wq_1989_2012" = min_flow_obs_wq_1989_2012,
                            "max_flow_obs_wq_2013_2022" = max_flow_obs_wq_2013_2022,
                            "min_flow_obs_wq_2013_2022" = min_flow_obs_wq_2013_2022)
  
  max_min_new1 <- max_min_new %>%
    pivot_longer(cols=3:length(max_min_new))
  
  max_min <- max_min %>%
    bind_rows(max_min_new1)
  
}

write.csv(max_min, "03_Results/max_min.csv", row.names=FALSE)
