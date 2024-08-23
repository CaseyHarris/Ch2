library(tidyverse)

flow_Alaf_Hills <- read.csv("flow_Alaf_Hills.csv")
flow_Jason1 <- read.csv("flow_Jason1.csv")

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
  } else if (str_detect(title, "Morris")) {
    flow0 <- flow_Alaf_Hills %>%
      filter(site=="Hillsborough") %>%
      select(Date, Q_cfs_log_round)
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

  max_min_new <- data.frame("title"=title, "max_flow_ever" = max_flow_ever,
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
    pivot_longer(cols=2:13)
  
  max_min <- max_min %>%
    bind_rows(max_min_new1)
  
}

write.csv(max_min, "03_Results/max_min.csv", row.names=FALSE)
