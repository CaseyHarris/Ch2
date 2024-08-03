#This section is just organizing the flow data

#Additional flow data (past) and future flow
all_flow <- read.csv("01_Data/pH Hillsborough R near Zephyrhills.csv")
all_flow <- all_flow %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date>=as.Date("2010-01-01"))
# jason_flow <- read.csv("Data/Jason_flow/Chang data business as usual.csv")
# jason_flow <- jason_flow %>%
#   mutate(date = as.Date(date)) #future1 is for some reason missing the very last day
# NOTE TO SELF: update to use this flow data later

CM3_1 <- read.csv("01_Data/Hills_CM3_future1.csv")
CM3_2 <- read.csv("01_Data/Hills_CM3_future2.csv")
ESM2G_1 <- read.csv("01_Data/Hills_ESM2G_future1.csv")
ESM2G_2 <- read.csv("01_Data/Hills_ESM2G_future2.csv")

flow_por <- all_flow %>%
  mutate(date = as.Date(Date),
         flow = Q,
         log_flow = log10(Q)) %>%
  select(date, flow, log_flow) %>%
  left_join(data0) %>% #change to data0 if using full dataset
  mutate(row = row_number(),
         day_of_year = yday(date),
         per="past")

CM3_1a <- CM3_1 %>%
  mutate(date = seq.Date(from=as.Date("2030-01-01"), to=as.Date("2060-12-29"), by="day")) %>%
  mutate(flow = X162.36/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="CM3_1") %>%
  select(-X162.36)

CM3_2a <- CM3_2 %>%
  mutate(date = seq.Date(from=as.Date("2070-01-01"), to=as.Date("2100-12-30"), by="day")) %>%
  mutate(flow = X162.79/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="CM3_2") %>%
  select(-X162.79)

ESM2G_1a <- ESM2G_1 %>%
  mutate(date = seq.Date(from=as.Date("2030-01-01"), to=as.Date("2060-12-29"), by="day")) %>%
  mutate(flow = X163.97/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="ESM2G_1") %>%
  select(-X163.97)

minESM2G_2a <- min(filter(ESM2G_2, X162.28>0))
ESM2G_2a <- ESM2G_2 %>%
  mutate(X162.28 = case_when(X162.28==0 ~ minESM2G_2a,
                             TRUE ~ X162.28)) %>%
  mutate(date = seq.Date(from=as.Date("2070-01-01"), to=as.Date("2100-12-30"), by="day")) %>%
  mutate(flow = X162.28/35.31466621266132,
         log_flow = log10(flow),
         wq = NA_real_,
         log_wq = NA_real_,
         row = seq(1, 11321),
         day_of_year = yday(date),
         per="ESM2G_2") %>%
  select(-X162.28)

data_by_day <- flow_por %>%
  bind_rows(CM3_1a) %>%
  bind_rows(CM3_2a) %>%
  bind_rows(ESM2G_1a) %>%
  bind_rows(ESM2G_2a) #%>% 
#mutate(log_flow = round(log_flow, digits=1)) #include if using dataframe with empty values