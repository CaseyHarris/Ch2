#FLOW

#This script retrieves USGS flow data,
#loads Chang et al. streamflow data,
#edits both to include logged/rounded flow values,
#and creates/saves a list of the min to max flow 
#values with a step of .001 on the log scale.
#These will be the flow values that 
#subsequent scripts will estimate
#water quality values for.

library(tidyverse)
library(dataRetrieval)

#USGS flow data
Alaf_flow <- readNWISdv(siteNumbers = "02301500",
                       parameterCd = "00060")
Hills_flow <- readNWISdv(siteNumbers = "02303330",
                              parameterCd = "00060")

Alaf_flow1 <- Alaf_flow %>%
  mutate(site = "Alafia",
         Date = as.Date(Date),
         Q_cfs = X_00060_00003) %>% #this is cfs
  select(site, Date, Q_cfs)
Hills_flow1 <- Hills_flow %>%
  mutate(site = "Hillsborough",
         Date = as.Date(Date),
         Q_cfs = X_00060_00003) %>%
  select(site, Date, Q_cfs)

#CHANG et al. flow data
flow_Jason <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Chang data business as usual.csv")
flow_Jason <- flow_Jason %>%
  mutate(Date = as.Date(date),
         Q_cfs = flow)

# Comparing USGS and Chang flow data
# unique(flow_Jason$scenario)
# flow_comp_Chang <- flow_Jason %>%
#   filter(site=="Hillsborough" & scenario=="NLDAS-2_Hargreaves")
# flow_comp_USGS <- Hills_flow1 %>%
#   filter(Date>=as.Date("1989-01-01") & Date<=as.Date("2012-12-13"))
# ggplot(flow_comp_USGS, aes(Date, Q_cfs)) +
#   geom_line() +
#   geom_line(data=flow_comp_Chang, aes(Date, Q_cfs), color="red")
# ***I guess NLDAS and USGS are not a great match***
# rm(flow_comp_Chang)
# rm(flow_comp_USGS)

#Saving USGS data and getting max/min
flow_Alaf_Hills <- Alaf_flow1 %>%
  bind_rows(Hills_flow1) %>%
  mutate(Q_cfs_log = log(Q_cfs)) %>%
  mutate(Q_cfs_log_round = round(Q_cfs_log, 3)) %>% #logs the values then rounds to the nearest .001
  mutate(scenario = "USGS") %>%
  select(site, scenario, Date, Q_cfs, Q_cfs_log, Q_cfs_log_round) %>%
  filter(!is.na(Q_cfs))
write.csv(flow_Alaf_Hills, "C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv", row.names=FALSE)
flow_Alaf_Hills <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Alaf_Hills.csv")
unique(flow_Alaf_Hills$Q_cfs) #2236
unique(flow_Alaf_Hills$Q_cfs_log_round) #2236
min(flow_Alaf_Hills$Q_cfs_log_round) #1.411
max(flow_Alaf_Hills$Q_cfs_log_round) #10.616

#Saving Chang et al. data and getting max/min
Jason_Hills <- flow_Jason %>%
  filter(site=="Hillsborough")
min_Jason_Hills <- min(Jason_Hills$Q_cfs[Jason_Hills$Q_cfs!=0])
Jason_Alaf <- flow_Jason %>%
  filter(site=="Alafia")
min_Jason_Alaf <- min(Jason_Alaf$Q_cfs[Jason_Alaf$Q_cfs!=0])
flow_Jason1 <- flow_Jason %>%
  mutate(Q_cfs_nonzero = case_when(Q_cfs==0 & site=="Hillsborough" ~ min_Jason_Hills, #replacing zeros with lowest non-zero flow
                                   Q_cfs==0 & site=="Alafia" ~ min_Jason_Alaf,
                                   TRUE ~ Q_cfs)) %>%
  mutate(Q_cfs_log = log(Q_cfs_nonzero)) %>%
  mutate(Q_cfs_log_round = round(Q_cfs_log, 3))
write.csv(flow_Jason1, "C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv", row.names=FALSE)
flow_Jason1 <- read.csv("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Data/flow_Jason1.csv")
unique(flow_Jason1$Q_cfs) #1505373
unique(flow_Jason1$Q_cfs_log_round) #11192, such a big difference, but still assuming Q is continuous
min(flow_Jason1$Q_cfs_log_round) #-8.082
max(flow_Jason1$Q_cfs_log_round) #10.481

#Saving a list of the full flow range of interest for use in further scripts
full_flow_range <- data.frame("Q_cfs_log_round" = seq(from=-8.082, to=10.616, by=.001))
full_flow_range <- full_flow_range %>%
  mutate(Q_cfs_log_round = as.numeric(as.character(Q_cfs_log_round)))
unique(full_flow_range$Q_cfs_log_round) #18699
#write.csv(full_flow_range, "01_Data/full_flow_range.csv", row.names=FALSE)

