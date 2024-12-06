#on sampling dates where more than one depth was sampled for a given analyte,
#the sample from the depth closest to the mode sampling depth over the period of record
#for that analyte at that site was selected.
#any samples from the same depth on the same day were averaged

#remove outliers

library(tidyverse)

data0 <- read.csv("01_Data/Data sites filtered but not depths.csv")

# pick_dates <- data_nut0 %>%
#   select(Name, ACTIVITY_START_TS, ANALYTE_GROUP, ORG_RESULT_edited) %>%
#   mutate(ACTIVITY_START_TS = as.Date(ACTIVITY_START_TS))
# 
# for (i in unique(data_nut0$ANALYTE_GROUP)) {
#   
#   data_nut_sub <- filter(data_nut0, ANALYTE_GROUP==i)
#   
#   ggplot(data_nut_sub, aes(as.Date(ACTIVITY_START_TS), ORG_RESULT_edited)) +
#     geom_point() +
#     facet_wrap(~Name)
#   ggsave(paste0("picking dates ", i, ".png", sep=""), width=8, height=7)
#   
# }

# dates <- read.csv("Pre-EGRET data/Dates.csv")
# dates <- dates %>%
#   mutate(dataset_name = paste0(Name, " ", ANALYTE_GROUP, sep="")) %>%
#   filter(ANALYTE_GROUP!="Total nitrogen as N")
# 
# data1 <- data0 %>%
#   left_join(dates, by=c("Name", "USGS", "ANALYTE_GROUP")) %>%
#   filter(!is.na(dataset_name)) %>%
#   mutate(date = as.Date(ACTIVITY_START_TS),
#          WQ_SFN_start = as.Date(WQ_SFN_start, format="%m/%d/%Y"),
#          WQ_SFN_end = as.Date(WQ_SFN_end, format="%m/%d/%Y")) %>%
#   filter(date>=WQ_SFN_start & date<=WQ_SFN_end)

# for (i in unique(data1$ANALYTE_GROUP)) {
#   
#   data1_sub <- filter(data1, ANALYTE_GROUP==i)
#   
#   ggplot(data1_sub, aes(date, ORG_RESULT_edited, color=ANALYTE_NAME)) +
#   geom_point() +
#   facet_wrap(~Name)
#   ggsave(paste0("comparing analyte names ", i, ".png", sep=""), width=9, height=7)
#   
# }

# unique(data1$ANALYTE_NAME)
# #DOM, pHM, CondM, TempWaterM
# data2 <- data1 %>%
#   mutate(contains_M = case_when(ANALYTE_NAME=="DOM" | ANALYTE_NAME=="pHM" | ANALYTE_NAME=="CondM" | ANALYTE_NAME=="TempWaterM" ~ 1,
#                                 TRUE ~ 0))
# samples_list <- data2 %>%
#   group_by(date, Name, USGS, ANALYTE_GROUP, dataset_name) %>%
#   summarise(avail_M = sum(contains_M))
# 
# data3 <- data2 %>%
#   left_join(samples_list) %>%
#   mutate(remove = case_when(avail_M==1 & contains_M==0 ~ "remove",
#                             TRUE ~ "keep")) %>%
#   filter(remove=="keep") %>% #keep only the M samples on dates when available
#   mutate(DEPTH = case_when(ANALYTE_NAME=="DOM" | ANALYTE_NAME=="pHM" | ANALYTE_NAME=="CondM" | ANALYTE_NAME=="TempWaterM" ~ as.numeric(ALT_DEPTH),
#                            TRUE ~ ACTIVITY_DEPTH))

data1 <- data0 %>%
  mutate(dataset_name = paste0(Name, " ", ANALYTE_GROUP2, sep="")) %>%
  mutate(date = as.Date(ACTIVITY_START_TS)) %>%
  group_by(date, Name, USGS, ANALYTE_GROUP2, dataset_name) %>% #if they're at the same depth, average them
  summarise(RESULT = mean(ORG_RESULT_edited),
            MDL = mean(ORG_MDL_edited, na.rm=TRUE, nan.rm=TRUE))
  
# samples_list <- data4 %>%
#   group_by(date, Name, USGS, ANALYTE_GROUP2, dataset_name) %>%
#   summarise(n = n())

data2 <- data1 %>%
  group_by(date, Name, USGS, ANALYTE_GROUP2, dataset_name) %>% #if there are multiple depths, average them
  summarise(RESULT = mean(RESULT),
            MDL = mean(MDL, na.rm=TRUE, nan.rm=TRUE)) %>%
  ungroup() %>%
  mutate(detect_status = case_when((is.na(MDL) | is.nan(MDL)) ~ "detected",
                                   RESULT>MDL & (!is.na(MDL) & !is.nan(MDL)) ~ "detected",
                                   RESULT==MDL ~ "non-detect",
                                   RESULT<MDL & (!is.na(MDL) & !is.nan(MDL)) ~ "non-detect",
                                   TRUE ~ "check"))

mdl_summary <- data2 %>%
  group_by(ANALYTE_GROUP2, detect_status) %>%
  summarise(n = n())

#value, remark, mdl, unit
data3 <- data2 %>%
  mutate(remark = case_when(detect_status=="non-detect" ~ "<",
                            TRUE ~ NA_character_),
         value = RESULT) %>%
  mutate(value = case_when(remark=="<" & RESULT<=0 ~ MDL,
                           TRUE ~ value)) %>%
  select(date, Name, USGS, ANALYTE_GROUP2, remark, value)

library(lubridate)
data4 <- data3 %>%
  filter(date>=as.Date("2013-01-01 00:00:00"))

summary <- data4 %>%
  mutate(year1 = year(date),
         year2 = year(date)) %>%
  mutate(year1 = year(date),
         year2 = year(date)) %>%
  group_by(Name, ANALYTE_GROUP2) %>%
  summarise(min = min(year1),
            max = max(year2),
            n = n())

unique(data4$ANALYTE_GROUP2)

write.csv(data4, "Data ready.csv", row.names=FALSE)

#Did not use below here, could return and try to improve

i="Alkalinity"
for (i in unique(data0$ANALYTE_GROUP)) {
  use <- filter(data0, ANALYTE_GROUP==i)
  ggplot(use, aes(date, ORG_RESULT_edited, shape=ANALYTE_NAME, color=ACTIVITY_DEPTH)) +
    geom_point() +
    ggtitle(i) +
    facet_wrap(~Name, scales="free_y", ncol=2) +
    theme(legend.position="right")
  ggsave(paste0("Figs selected dates/", i, ".png"), width=15, height=12)
}

#HERE'S WHERE I LOSE METADATA
#Now I need to look for duplicated samples at a given depth/site/day
data1 <- data0 %>%
  group_by(date, Name, USGS, ANALYTE_GROUP, ACTIVITY_DEPTH, unit, dataset_name) %>%
  summarise(n = n(),
            result = mean(ORG_RESULT_edited, na.rm=TRUE),
            mdl = mean(ORG_MDL_edited, na.rm=TRUE),
            detect_status = paste0(detect_status, collapse="")) %>%
  ungroup() %>%
  mutate(detect_status = case_when(detect_status=="below MDLfine" ~ "fine",
                            detect_status=="finebelow MDL" ~ "fine",
                            TRUE ~ detect_status)) %>%
  mutate(detect_status = case_when(detect_status=="finefine" | detect_status=="finefinefinefine" ~ "fine",
                                   detect_status=="below MDLbelow MDL" ~ "below MDL",
                                   detect_status=="above UDL" | detect_status=="above UDLabove UDL" ~ "fine",
                            TRUE ~ detect_status))

unique(data1$detect_status)

data2 <- data1 %>%
  group_by(date, Name, USGS, ANALYTE_GROUP, unit, dataset_name) %>%
  summarise(n = n(),
            result= mean(result),
            mdl = mean(mdl, na.rm=TRUE),
            detect_status = paste0(detect_status, collapse="")) %>%
  ungroup()

#value, remark, mdl, unit
data3 <- data2 %>%
  mutate(remark = case_when(detect_status=="below MDL" ~ "<",
                                   TRUE ~ NA_character_),
         value = result) %>%
  mutate(value = case_when(remark=="<" ~ mdl,
                           TRUE ~ value)) %>%
  select(date, Name, USGS, ANALYTE_GROUP, remark, value, unit)

write.csv(data3, "Re-download/Used/data for today.csv", row.names=FALSE)

#DID NOT USE BELOW HERE
#STOPPING HERE, need to return and fix depths, zero values, flow dates
data5 <- data1 %>%
  mutate(ACTIVITY_DEPTH1 = round(ACTIVITY_DEPTH, 1)) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP, ORG_MDL_edited, detect_status, unit, ACTIVITY_DEPTH1) %>%
  summarise(ORG_RESULT_edited = mean(ORG_RESULT_edited)) %>%
  ungroup()

data_temp <- data1 %>%
  group_by(date, Name, USGS, ANALYTE_GROUP) %>%
  summarise(value = mean(ORG_RESULT_edited, na.rm=TRUE),
            mdl = mean(ORG_MDL_edited, na.rm=TRUE))%>%
  ungroup() %>%
  mutate(remark = case_when(!is.na(mdl) & value<mdl ~ "<",
                            TRUE ~ NA_character_)) %>%
  select(date, Name, USGS, ANALYTE_GROUP, remark, value) %>%
  filter(value>0)
write.csv(data_temp, "data for today.csv", row.names=FALSE)

#looking for duplicate samples at the same depth with different MDLs and same MDL status
#averaged the samples and the MDLs (many samples had this situatiion)
data7 <- data5 %>%
  group_by(USGS, Name, date, ANALYTE_GROUP, below_MDL, unit_edit, ACTIVITY_DEPTH1) %>%
  summarise(ORG_RESULT_edited3 = mean(ORG_RESULT_edited2),
            MDL_edit_new = mean(ORG_MDL, na.rm=TRUE, nan.rm=TRUE)) %>%
  ungroup() %>%
  mutate(below_MDL_new = case_when(ORG_RESULT_edited3<MDL_edit_new ~ "yes",
                                   TRUE ~ "no"))

ggplot(data7, aes(date, result_edit_new)) +
  geom_point(shape=1) +
  facet_grid(ANALYTE_GROUP ~ long_name, scales="free")

#looking for duplicate samples at the same depth with different MDLs and any MDL status
data_temp <- data7 %>%
  group_by(USGS, Name, date, ANALYTE_GROUP, unit_edit, ACTIVITY_DEPTH1) %>%
  summarise(n = n())
#no samples had this situation

data8 <- data7 %>%
  group_by(USGS, Name, date, ANALYTE_GROUP, unit_edit, ACTIVITY_DEPTH1) %>%
  summarise(ORG_RESULT_edited3 = mean(ORG_RESULT_edited3),
            MDL_edit_new = mean(MDL_edit_new, na.rm=TRUE, nan.rm=TRUE)) %>%
  ungroup() %>%
  mutate(below_MDL_new = case_when(ORG_RESULT_edited3<MDL_edit_new ~ "yes",
                                   TRUE ~ "no"))

ggplot(data8, aes(date, ORG_RESULT_edited3)) +
  geom_point(shape=1) +
  facet_grid(ANALYTE_GROUP ~ Name, scales="free")

#in cases where no MDL was provided, data was assumed to be above it!
data_temp <- data8 %>%
  filter(is.na(MDL_edit_new) | is.nan(MDL_edit_new)) %>%
  group_by(ANALYTE_GROUP) %>%
  summarise(n = n())

#method for preserving metadata
# data3 <- data2 %>%
#   group_by(USGS_name, long_name, ANALYTE_NAME1, ACTIVITY_DEPTH, date) %>%
#   summarise(VALUE = mean(ORG_RESULT_VALUE),
#             DATA_SOURCES = paste0(DATA_SOURCE, collapse=", "),
#             ORGANIZATION_IDS = paste0(ORGANIZATION_ID, collapse=", "),
#             WIN_MONITORING_LOC_IDS = paste0(WIN_MONITORING_LOC_ID, collapse=", "),
#             STORET_MONITORING_LOC_IDS = paste0(STORET_MONITORING_LOC_ID, collapse=", "),
#             WIN_ACTIVITY_IDS = paste0(WIN_ACTIVITY_ID, collapse=", "),
#             ACTIVITY_TYPES = paste0(ACTIVITY_TYPE, collapse=", "),
#             RESULT_IDS = paste0(RESULT_ID, collapse=", "),
#             ACTIVITY_DEPTH_UNIT = "m",
#             ORIGINAL_ANALYTE_NAMES = paste0(ANALYTE_NAME, collapse=", "),
#             ORG_RESULT_VALUES = paste0(ORG_RESULT_VALUE, collapse=", "),
#             ORG_MDLS = paste0(ORG_MDL, collapse=", "),
#             ORG_PQLS = paste0(ORG_PQL, collapse=", "),
#             DETECTION_TEXT = paste0(DETECTION_TEXT, collapse=", "),
#             VALUE_QUALIFIERS = paste0(VALUE_QUALIFER, collapse=", "),
#             ANALYSIS_METHODS = paste0(ANALYSIS_METHOD, collapse=", "),
#             MON_LOC_NAMES = paste0(MON_LOC_NAME, collapse=", "),
#             DEP.Latitudes = paste0(DEP.Latitude, collapse=", "),
#             DEP.Longitudes = paste0(DEP.Longitude, collapse=", "),
#             short_names = paste0(short_name, collapse=", ")) %>%
#   ungroup()

#Now will work on duplicates measured at different depths
depth_modes <- data8 %>%
  #filter(ANALYTE_GROUP!="Temperature") %>%
  group_by(USGS, Name, ANALYTE_GROUP) %>%
  mutate(n_samples = n()) %>%
  ungroup() %>%
  group_by(USGS, Name, ANALYTE_GROUP, ACTIVITY_DEPTH1) %>%
  summarise(n_at_depth = n(),
            n_samples = mean(n_samples)) %>%
  ungroup() %>%
  filter(!is.na(ACTIVITY_DEPTH1)) %>%
  group_by(USGS, Name, ANALYTE_GROUP, n_samples) %>%
  summarise(mode_depth = ACTIVITY_DEPTH1[n_at_depth==max(n_at_depth)],
            mode_n = n_at_depth[n_at_depth==max(n_at_depth)]) %>%
  ungroup()

write.csv(depth_modes, "Re-download/depths_to_use.csv", row.names=FALSE)

depths_to_use <- read.csv("Re-download/depths_to_use.csv")

data9 <- data8 %>%
  left_join(depths_to_use, by=c("USGS", "Name", "ANALYTE_GROUP")) %>%
  mutate(preferred_depth = mode_depth) %>%
  mutate(preferred_depth1 = case_when(ACTIVITY_DEPTH1==preferred_depth ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(depth_diff = round(abs(ACTIVITY_DEPTH1 - preferred_depth), 1)) %>%
  mutate(preferred_depth1 = case_when(is.na(depth_diff) ~ 1,
                                      TRUE ~ preferred_depth1),
         depth_diff = case_when(is.na(depth_diff) ~ 0,
                                TRUE ~ depth_diff)) %>%
  mutate(preferred_depth2 = case_when(preferred_depth1==0 & depth_diff==.1 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(preferred_depth3 = case_when(preferred_depth1==0 & preferred_depth2==0 & depth_diff==.2 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(preferred_depth4 = case_when(preferred_depth1==0 & preferred_depth2==0 & preferred_depth3==0 & depth_diff==.3 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(preferred_depth5 = case_when(preferred_depth1==0 & preferred_depth2==0 & preferred_depth3==0 & preferred_depth4==0 & depth_diff==.4 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(preferred_depth6 = case_when(preferred_depth1==0 & preferred_depth2==0 & preferred_depth3==0 & preferred_depth4==0 & preferred_depth5==0 & depth_diff==.5 ~ 1,
                                      TRUE ~ 0)) %>%
  mutate(preferred_depth7 = case_when(preferred_depth1==0 & preferred_depth2==0 & preferred_depth3==0 & preferred_depth4==0 & preferred_depth5==0 & preferred_depth6==0 & depth_diff>.5 ~ 1,
                                      TRUE ~ 0))

dates_list <- data9 %>%
  group_by(USGS, Name, date, ANALYTE_GROUP, preferred_depth) %>%
  summarise(ORG_RESULT_edited = NA) %>%
  ungroup()

d1 <- data9 %>%
  filter(preferred_depth1==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited1 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d2 <- data9 %>%
  filter(preferred_depth2==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited2 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d3 <- data9 %>%
  filter(preferred_depth3==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited3 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d4 <- data9 %>%
  filter(preferred_depth4==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited4 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d5 <- data9 %>%
  filter(preferred_depth5==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited5 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d6 <- data9 %>%
  filter(preferred_depth6==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited6 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

d7 <- data9 %>%
  filter(preferred_depth7==1) %>%
  group_by(USGS, Name, date, ANALYTE_GROUP) %>%
  summarise(ORG_RESULT_edited7 = mean(ORG_RESULT_edited3, na.rm=TRUE)) %>%
  ungroup()

length(d1$date) +length(d2$date) +length(d3$date) +length(d4$date) +length(d5$date) +length(d6$date) +length(d7$date)

data10 <- dates_list %>%
  left_join(d1) %>%
  left_join(d2) %>%
  left_join(d3) %>%
  left_join(d4) %>%
  left_join(d5) %>%
  left_join(d6) %>%
  left_join(d7) %>%
  mutate(ORG_RESULT_edited = case_when(!is.na(ORG_RESULT_edited1) ~ ORG_RESULT_edited1,
                                       is.na(ORG_RESULT_edited1) & !is.na(ORG_RESULT_edited2) ~ ORG_RESULT_edited2,
                                       is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & !is.na(ORG_RESULT_edited3) ~ ORG_RESULT_edited3,
                                       is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & !is.na(ORG_RESULT_edited4) ~ ORG_RESULT_edited4,
                                       is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & !is.na(ORG_RESULT_edited5) ~ ORG_RESULT_edited5,
                                       is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & is.na(ORG_RESULT_edited5) & !is.na(ORG_RESULT_edited6) ~ ORG_RESULT_edited6,
                                       is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & is.na(ORG_RESULT_edited5) & is.na(ORG_RESULT_edited6) & !is.na(ORG_RESULT_edited7) ~ ORG_RESULT_edited7,
                                       TRUE ~ NA)) %>%
  mutate(depth_from_pref = case_when(!is.na(ORG_RESULT_edited1) ~ "at preferred depth",
                                     is.na(ORG_RESULT_edited1) & !is.na(ORG_RESULT_edited2) ~ "0.1 m from preferred",
                                     is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & !is.na(ORG_RESULT_edited3) ~ "0.2 m from preferred",
                                     is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & !is.na(ORG_RESULT_edited4) ~ "0.3 m from preferred",
                                     is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & !is.na(ORG_RESULT_edited5) ~ "0.4 m from preferred",
                                     is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & is.na(ORG_RESULT_edited5) & !is.na(ORG_RESULT_edited6) ~ "0.5 m from preferred",
                                     is.na(ORG_RESULT_edited1) & is.na(ORG_RESULT_edited2) & is.na(ORG_RESULT_edited3) & is.na(ORG_RESULT_edited4) & is.na(ORG_RESULT_edited5) & is.na(ORG_RESULT_edited6) & !is.na(ORG_RESULT_edited7) ~ ">0.5 m from preferred",
                                     TRUE ~ NA)) %>%
  select(USGS, Name, date, ANALYTE_GROUP, ORG_RESULT_edited, depth_from_pref)

write.csv(data10, "Data with selected depths.csv", row.names=FALSE)

ggplot(data10, aes(date, ORG_RESULT_edited, color=depth_from_pref)) +
  geom_point(shape=1) +
  scale_color_manual(values=c("red", "blue", "blue", "blue", "blue", "blue", "green")) +
  facet_wrap(ANALYTE_GROUP ~ Name, scales="free")

#NOT USED BELOW HERE

#data from the depths shown in the table was used when available
#on dates when data was not available at those depths, 
# -any remaining data available from 0.2 or 0.3 meters was used for the
#  Alafia R. North Prong, Alafia R. South Prong, Alafia R. mainstem, Trout Creek, and Blackwater Creek,
#  after that, any remaining data from .5 m or less was averaged and used
#  and then data with no depth recorded was used
# -data from recorded depths greater than .5 m was not used
#
# -data from the Hillsborough R and Cypress Creek sites,
#  any remaining data from 0.5 meters or less was averaged and used
#  and then data with no depth recorded was used
# -data from recorded depths greater than .5 m was not used

# i="Hillsborough R. at Morris Br."
# for (i in unique(data9$long_name)) {
#   data_use <- filter(data9, long_name==i)#, ACTIVITY_DEPTH<=.5)
#   ggplot(data_use, aes(date, result_edit_new, color=ACTIVITY_DEPTH)) +
#     geom_point(shape=1) +
#     facet_wrap(~ANALYTE_GROUP, scales="free_y", ncol=1)
#   ggsave(paste0(i, "all.png", sep=""), width=11, height=8)
# }

#need to get 23,152
rows <- data9 %>%
  group_by(USGS_site, long_name, date, ANALYTE_NAME2, ANALYTE_GROUP, unit_edit, row_id) %>%
  summarise(n = n()) %>%
  ungroup()

data_depths1 <- data9 %>%
  filter(preferred_depth1==1) %>%
  group_by(USGS_site, long_name, date, ANALYTE_NAME2, ANALYTE_GROUP, unit_edit, row_id) %>%
  summarise(result_edit_new = mean(result_edit_new),
            MDL_edit_new = mean(MDL_edit_new, na.rm=TRUE, nan.rm=TRUE))%>%
  ungroup()

rows1 <- rows %>%
  left_join(data_depths1)

rows_remaining <- rows1 %>%
  filter(is.na(result_edit_new)) %>%
  select(-result_edit_new, -MDL_edit_new)

rows1 <- rows1 %>%
  filter(!is.na(result_edit_new))

data_depths2 <- data9 %>%
  filter(preferred_depth2==1) %>%
  filter(row_id %in% rows_remaining$row_id == TRUE) %>%
  group_by(USGS_site, long_name, date, ANALYTE_NAME2, ANALYTE_GROUP, unit_edit, row_id) %>%
  summarise(result_edit_new = mean(result_edit_new),
            MDL_edit_new = mean(MDL_edit_new, na.rm=TRUE, nan.rm=TRUE))%>%
  ungroup()

rows2 <- rows_remaining %>%
  left_join(data_depths2)

rows_remaining <- rows2 %>%
  filter(is.na(result_edit_new)) %>%
  select(-result_edit_new, -MDL_edit_new)

rows2 <- rows2 %>%
  filter(!is.na(result_edit_new))

data_depths3 <- data9 %>%
  filter(preferred_depth3==1) %>%
  filter(row_id %in% rows_remaining$row_id == TRUE) %>%
  group_by(USGS_site, long_name, date, ANALYTE_NAME2, ANALYTE_GROUP, unit_edit, row_id) %>%
  summarise(result_edit_new = mean(result_edit_new),
            MDL_edit_new = mean(MDL_edit_new, na.rm=TRUE, nan.rm=TRUE))%>%
  ungroup()

rows3 <- rows_remaining %>%
  left_join(data_depths3)

rows_remaining <- rows3 %>%
  filter(is.na(result_edit_new)) %>%
  select(-result_edit_new, -MDL_edit_new)

rows3 <- rows3 %>%
  filter(!is.na(result_edit_new))

data_depths4 <- data9 %>%
  filter(preferred_depth4==1) %>%
  filter(row_id %in% rows_remaining$row_id == TRUE) %>%
  group_by(USGS_site, long_name, date, ANALYTE_NAME2, ANALYTE_GROUP, unit_edit, row_id) %>%
  summarise(result_edit_new = mean(result_edit_new),
            MDL_edit_new = mean(MDL_edit_new, na.rm=TRUE, nan.rm=TRUE))%>%
  ungroup()

rows4 <- rows_remaining %>%
  left_join(data_depths4)

rows_remaining <- rows4 %>%
  filter(is.na(result_edit_new)) %>%
  select(-result_edit_new, -MDL_edit_new)

rows4 <- rows4 %>%
  filter(!is.na(result_edit_new))

length(data_depths1$date) + length(data_depths2$date) + length(data_depths3$date) + length(data_depths4$date)

all_rows <- data_depths1 %>%
  bind_rows(data_depths2) %>%
  bind_rows(data_depths3) %>%
  bind_rows(data_depths4) %>%
  mutate(below_MDL = case_when(result_edit_new<MDL_edit_new & !is.na(MDL_edit_new) & !is.nan(MDL_edit_new) ~ "yes",
                               result_edit_new>=MDL_edit_new & !is.na(MDL_edit_new) & !is.nan(MDL_edit_new) ~ "no", 
                               is.na(MDL_edit_new) | is.nan(MDL_edit_new) ~ "MDL missing"))

ggplot(all_rows, aes(date, result_edit_new, color=below_MDL)) +
  geom_point(shape=1) +
  facet_grid(ANALYTE_GROUP ~ long_name, scales="free")

write.csv(all_rows, "2_Intermediate_data/data_filtered_depths.csv", row.names=FALSE)
