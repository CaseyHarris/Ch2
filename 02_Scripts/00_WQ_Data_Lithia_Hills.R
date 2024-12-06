library(tidyverse)
library(readxl)

#Load data from DEP (STORET and WIN systems)
STORET0 <- read_excel("01_Data/Huc_pull.xlsx", col_types=c("text", "text", "text", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
WIN0 <- read_excel("01_Data/Huc_pull2.xlsx", col_types=c("text", "text", "text", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

#STORET
STORET1 <- STORET0 %>%
  select(DATA_SOURCE, ORGANIZATION_ID, WIN_MONITORING_LOC_ID, STORET_MONITORING_LOC_ID,
         ACTIVITY_START_TS, ACTIVITY_TYPE, ACTIVITY_DEPTH, ACTIVITY_DEPTH_UNIT,
         ANALYTE_NAME, ORG_RESULT_VALUE, ORG_RESULT_UNIT, ORG_MDL, ORG_PQL,
         ORG_DETECTION_UNIT, VALUE_QUALIFER, MON_LOC_NAME)

stations_STORETa <- read_excel("01_Data/Stations_excel_STORET1.xlsx", col_types="text")
stations_STORETb <- read_excel("01_Data/Stations_excel_STORET2.xlsx", col_types="text")

colnames(stations_STORETa) <- make.names(colnames(stations_STORETa))
colnames(stations_STORETb) <- make.names(colnames(stations_STORETb))

stations_STORET <- stations_STORETa %>%
  bind_rows(stations_STORETb) %>%
  select(Org.ID, Org.Name, Station.ID, Station.Name, County, WBID,
         Latitude, Longitude, Primary.Type, Secondary.Type, Datum, HUC)

unique(stations_STORET$Station.ID) 
STORET2 <- STORET1 %>%
  left_join(stations_STORET, by=c("STORET_MONITORING_LOC_ID"="Station.ID"))

STORET3 <- STORET2 %>%
  filter(!is.na(Latitude)) #Some stations do not have Station.IDs (and thus will not have lat/lon); I have checked these, and they are not needed

#This changes the minute/degrees lat/lon into decimal degrees
library(sp)
data_STORET_lat <- read.table(text=STORET3$Latitude)
data_STORET_lon <- read.table(text=STORET3$Longitude)
data_STORET_lat$string <- paste0(data_STORET_lat$V1, "d", data_STORET_lat$V2, "\'", data_STORET_lat$V3, '\"', " N", sep="")
data_STORET_lat$string
data_STORET_lat$numeric <- as.numeric(sp::char2dms(data_STORET_lat$string))
data_STORET_lat$numeric

data_STORET_lon$string <- paste0(data_STORET_lon$V1, "d", data_STORET_lon$V2, "\'", data_STORET_lon$V3, '\"', " W", sep="")
data_STORET_lon$string
data_STORET_lon$numeric <- as.numeric(sp::char2dms(data_STORET_lon$string))
data_STORET_lon$numeric

STORET4 <- STORET3 %>%
  mutate(Latitude = data_STORET_lat$numeric,
         Longitude = data_STORET_lon$numeric)

rm(data_STORET_lat)
rm(data_STORET_lon)
rm(STORET0)
rm(STORET1)
rm(STORET2)
rm(STORET3)
rm(stations_STORETa)
rm(stations_STORETb)
rm(stations_STORET)

#WIN
WIN1 <- WIN0 %>%
  select(DATA_SOURCE, ORGANIZATION_ID, WIN_MONITORING_LOC_ID, STORET_MONITORING_LOC_ID,
         ACTIVITY_START_TS, ACTIVITY_TYPE, ACTIVITY_DEPTH, ACTIVITY_DEPTH_UNIT,
         ANALYTE_NAME, ORG_RESULT_VALUE, ORG_RESULT_UNIT, ORG_MDL, ORG_PQL,
         ORG_DETECTION_UNIT, VALUE_QUALIFER, MON_LOC_NAME)

stations_WIN <- read_excel("01_Data/Stations_excel_WIN.xlsx", skip=6, col_types="text") #WIN
colnames(stations_WIN) <- make.names(colnames(stations_WIN))

WIN2 <- WIN1 %>%
  left_join(stations_WIN, by=c("WIN_MONITORING_LOC_ID"="Monitoring.Location.ID"))

rm(WIN0)
rm(WIN1)
rm(stations_WIN)

#Combining STORET and WIN data
STORET5 <- STORET4 %>%
  mutate(Latitude=as.character(Latitude),
         Longitude=as.character(Longitude)) %>%
  select(ACTIVITY_START_TS, ACTIVITY_TYPE, ACTIVITY_DEPTH, ACTIVITY_DEPTH_UNIT,
         ANALYTE_NAME, ORG_RESULT_VALUE, ORG_RESULT_UNIT, ORG_MDL, ORG_PQL,
         ORG_DETECTION_UNIT, VALUE_QUALIFER, MON_LOC_NAME, 
         Station.Name, County, WBID, Latitude, Longitude, 
         Primary.Type, Secondary.Type, HUC)

WIN3 <- WIN2 %>%
  mutate(Station.Name=Monitoring.Location.Name,
         Latitude=DEP.Latitude,
         Longitude=DEP.Longitude,
         HUC=HUC8.Value) %>%
  select(ACTIVITY_START_TS, ACTIVITY_TYPE, ACTIVITY_DEPTH, ACTIVITY_DEPTH_UNIT,
         ANALYTE_NAME, ORG_RESULT_VALUE, ORG_RESULT_UNIT, ORG_MDL, ORG_PQL,
         ORG_DETECTION_UNIT, VALUE_QUALIFER, MON_LOC_NAME, 
         Station.Name, County, WBID, Latitude, Longitude, 
         Primary.Type, Secondary.Type, HUC)

dep <- STORET5 %>%
  bind_rows(WIN3)

rm(STORET4)
rm(STORET5)
rm(WIN2)
rm(WIN3)

#Filter out unwanted location types (wells, lakes, springs)
unique(dep$Primary.Type)
unique(dep$Secondary.Type)

dep1 <- dep %>%
  filter(is.na(Primary.Type) | (Primary.Type!="Ground Water" & Primary.Type!="Lake" & Primary.Type!="Spring")) %>%
  filter(is.na(Secondary.Type) | (Secondary.Type!="Spring Vent" & Secondary.Type!="Spring Boil"))

unique(dep1$Primary.Type)
unique(dep1$Secondary.Type)

#Map the sites to determine which are located near USGS sites
library(leaflet)
library(htmltools)
#library(mapview)

dep2 <- dep1 %>%
  mutate(Latitude = round(as.numeric(Latitude), 5),
         Longitude = round(as.numeric(Longitude), 5)) #Round lat/lon to five digits

dep_map0 <- dep2 %>%
  group_by(Latitude, Longitude) %>%
  summarise(n = n()) %>%
  ungroup() #%>%
#filter(ANALYTE_NAME=="Specific Conductance")

dep_map1 <- dep_map0 %>%
  #mutate(short_name = as.character(1:length(dep_map0$n))) #1773
  mutate(lat_lon = paste0(as.character(Latitude), ", ", as.character(Longitude), sep=""))

m <- leaflet(dep_map1) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, label = ~htmlEscape(paste0(lat_lon, ", ", n, sep="")), labelOptions=labelOptions(noHide=F), clusterOptions = markerClusterOptions())
m #Selected sites


unique(dep2$ORG_RESULT_VALUE)
dep3 <- dep2 %>%
  filter(ANALYTE_NAME!="Depth, Secchi Disk Depth (Choice List)" &
           ANALYTE_NAME!="Secchi disk depth (choice list)" &
           ANALYTE_NAME!="Flow, severity (choice list)")

#Add EPC
EPC <- read_excel("01_Data/EPC.xlsx", sheet="RWMDataSpreadsheet", col_types="text")

EPC1 <- EPC
colnames(EPC1) <- make.names(colnames(EPC))
colnames(EPC1) <- gsub("\\.", "", colnames(EPC1))
colnames(EPC1) <- gsub("\\_", "", colnames(EPC1))
colnames(EPC1) <- sub("(.*)(Q)(.*)", "\\1\\3.\\2", colnames(EPC1))
colnames(EPC1)
EPC2 <- EPC1[1:20000,]
EPC3 <- EPC1[20001:40000,]
EPC4 <- EPC1[40001:60000,]
EPC5 <- EPC1[60001:73323,]

EPC_piece1 <- EPC2 %>%
  select(SampleTime, ProgramName, SampleDepth, AreaName, StationNumber, WBID, Latitude, Longitude, PrimaryStationType, HUC, Notes, DepthM, TempWaterT:TOCL.Q) %>%
  pivot_longer(cols=c(TempWaterT:TOCL.Q), names_to=c("Variable")) %>%
  separate(Variable, c("Variable", "Quality")) %>%
  mutate(Quality = case_when(is.na(Quality) ~ "Value",
                             TRUE ~ Quality)) %>%
  pivot_wider(names_from=Quality, values_from=value)
EPC_piece2 <- EPC3 %>%
  select(SampleTime, ProgramName, SampleDepth, AreaName, StationNumber, WBID, Latitude, Longitude, PrimaryStationType, HUC, Notes, DepthM, TempWaterT:TOCL.Q) %>%
  pivot_longer(cols=c(TempWaterT:TOCL.Q), names_to=c("Variable")) %>%
  separate(Variable, c("Variable", "Quality")) %>%
  mutate(Quality = case_when(is.na(Quality) ~ "Value",
                             TRUE ~ Quality)) %>%
  pivot_wider(names_from=Quality, values_from=value)
EPC_piece3 <- EPC4 %>%
  select(SampleTime, ProgramName, SampleDepth, AreaName, StationNumber, WBID, Latitude, Longitude, PrimaryStationType, HUC, Notes, DepthM, TempWaterT:TOCL.Q) %>%
  pivot_longer(cols=c(TempWaterT:TOCL.Q), names_to=c("Variable")) %>%
  separate(Variable, c("Variable", "Quality")) %>%
  mutate(Quality = case_when(is.na(Quality) ~ "Value",
                             TRUE ~ Quality)) %>%
  pivot_wider(names_from=Quality, values_from=value)
EPC_piece4 <- EPC5 %>%
  select(SampleTime, ProgramName, SampleDepth, AreaName, StationNumber, WBID, Latitude, Longitude, PrimaryStationType, HUC, Notes, DepthM, TempWaterT:TOCL.Q) %>%
  pivot_longer(cols=c(TempWaterT:TOCL.Q), names_to=c("Variable")) %>%
  separate(Variable, c("Variable", "Quality")) %>%
  mutate(Quality = case_when(is.na(Quality) ~ "Value",
                             TRUE ~ Quality)) %>%
  pivot_wider(names_from=Quality, values_from=value)

EPC6 <- EPC_piece1 %>%
  bind_rows(EPC_piece2) %>%
  bind_rows(EPC_piece3) %>%
  bind_rows(EPC_piece4)

rm(EPC)
rm(EPC1)
rm(EPC2)
rm(EPC3)
rm(EPC4)
rm(EPC5)
rm(EPC_piece1)
rm(EPC_piece2)
rm(EPC_piece3)
rm(EPC_piece4)

EPC7 <- EPC6 %>%
  mutate(non_num1 = case_when(str_detect(Value, ">") ~ ">",
                              TRUE ~ NA),
         non_num2 = case_when(str_detect(Value, "<") ~ "<",
                              TRUE ~ NA)) %>%
  mutate(Result_edit = Value) %>%
  mutate(Result_edit = gsub("<", "", Result_edit)) %>%
  mutate(Result_edit = gsub(">", "", Result_edit)) %>%
  mutate(Result_edit = as.numeric(Result_edit)) %>%
  mutate(Latitude = round(as.numeric(Latitude), digits=5),
         Longitude = round(as.numeric(Longitude), digits=5))

summ <- EPC7 %>%
  group_by(StationNumber, Latitude, Longitude) %>%
  summarise(n = n())

library(leaflet)
library(htmltools)
#library(mapview)
library(tidyverse)
m <- leaflet(summ) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addMarkers(lng = summ$Longitude, lat = summ$Latitude, label = ~htmlEscape(paste0(Latitude, ", ", Longitude, ", ", n, ", ", StationNumber, sep="")), labelOptions=labelOptions(noHide=F), clusterOptions = markerClusterOptions())
m
#Selected sites are in Sites file

EPC8 <- EPC7 %>%
  mutate(ACTIVITY_START_TS = as.POSIXct(SampleTime, format="%m/%d/%Y %H:%M"),
         ACTIVITY_TYPE = ProgramName,
         ACTIVITY_DEPTH = SampleDepth,
         ALT_DEPTH = DepthM,
         ACTIVITY_DEPTH_UNIT = "EPC",
         ANALYTE_NAME = Variable,
         ORG_RESULT_VALUE = Value,
         ORG_RESULT_UNIT = "EPC",
         ORG_MDL = case_when(non_num1=="<" | non_num2==">" ~ as.character(Result_edit),
                             TRUE ~ NA_character_),
         ORG_PQL = case_when(non_num1=="<" | non_num2==">" ~ as.character(Result_edit),
                             TRUE ~ NA_character_),
         ORG_DETECTION_UNIT = "EPC",
         VALUE_QUALIFER = Q,
         MON_LOC_NAME = AreaName,
         Station.Name = StationNumber,
         County = "EPC",
         WBID = WBID,
         Latitude = Latitude,
         Longitude = Longitude,
         Primary.Type = PrimaryStationType,
         Secondary.Type = "EPC",
         HUC = HUC,
         ORG_RESULT_edited = as.character(Result_edit)) %>%
  select(ACTIVITY_START_TS,
         ACTIVITY_TYPE,
         ACTIVITY_DEPTH,
         ACTIVITY_DEPTH_UNIT,
         ANALYTE_NAME,
         ORG_RESULT_VALUE,
         ORG_RESULT_UNIT,
         ORG_MDL,
         ORG_PQL,
         ORG_DETECTION_UNIT,
         VALUE_QUALIFER,
         MON_LOC_NAME,
         Station.Name,
         County,
         WBID,
         Latitude,
         Longitude,
         Primary.Type,
         Secondary.Type,
         HUC,
         ORG_RESULT_edited,
         non_num1,
         non_num2,
         Notes,
         ALT_DEPTH)

dep4 <- dep3 %>%
  mutate(ORG_RESULT_edited = ORG_RESULT_VALUE,
         non_num1 = "DEP",
         non_num2 = "DEP",
         Notes = "DEP",
         ALT_DEPTH = "DEP")

both0 <- dep4 %>%
  bind_rows(EPC8)

# both0_plot <- both0 %>%
#   filter(Station.Name=="108" | Station.Name=="114" | Station.Name=="115" | Station.Name=="116") %>%
#   filter(ANALYTE_NAME!="Arsenic" & ANALYTE_NAME!="Cadmium" & ANALYTE_NAME!="Calcium" & ANALYTE_NAME!="Chloride" & ANALYTE_NAME!="Chromium" & ANALYTE_NAME!="Copper" & ANALYTE_NAME!="Iron" & ANALYTE_NAME!="Lead" & ANALYTE_NAME!="Magnesium" & ANALYTE_NAME!="Manganese" & ANALYTE_NAME!="Nickel" & ANALYTE_NAME!="Potassium" & ANALYTE_NAME!="Sodium" & ANALYTE_NAME!="Zinc" &
#            ANALYTE_NAME!="Chlorophyllt" & ANALYTE_NAME!="Chlorophylla" & ANALYTE_NAME!="Chlorophyllb" & ANALYTE_NAME!="Chlorophyllc" & ANALYTE_NAME!="EColiform" & ANALYTE_NAME!="Enterococci" & ANALYTE_NAME!="FecalColiform" & ANALYTE_NAME!="ChlorophyllaCorr" & ANALYTE_NAME!="Chlorophyllpheo" &
#            ANALYTE_NAME!="DOpB" & ANALYTE_NAME!="DOpM" & ANALYTE_NAME!="DOpT" & ANALYTE_NAME!="DepthB" & ANALYTE_NAME!="DepthM" &
#            ANALYTE_NAME!="CondB" & ANALYTE_NAME!="CondT" & ANALYTE_NAME!="DOB" & ANALYTE_NAME!="DOT" & ANALYTE_NAME!="pHB" & ANALYTE_NAME!="pHT" & ANALYTE_NAME!="SalB" & ANALYTE_NAME!="SalT" & ANALYTE_NAME!="TempWaterB" & ANALYTE_NAME!="TempWaterT")
# 
# analytes <- unique(both0_plot$ANALYTE_NAME)
# analytes1 <- analytes[1:(length(analytes)/2)]
# analytes2 <- analytes[(length(analytes)/2+1):length(analytes)]
# 
# both0_plot1 <- both0_plot %>%
#   mutate(meas = case_when(!is.na(ORG_RESULT_VALUE) ~ 1,
#                           TRUE ~ 0)) %>%
#   filter(ANALYTE_NAME %in% analytes1)
# 
# both0_plot2 <- both0_plot %>%
#   mutate(meas = case_when(!is.na(ORG_RESULT_VALUE) ~ 1,
#                           TRUE ~ 0)) %>%
#   filter(ANALYTE_NAME %in% analytes2)
# 
# ggplot(both0_plot1, aes(ACTIVITY_START_TS, meas)) +
#   geom_point() +
#   facet_grid(ANALYTE_NAME ~ Station.Name)
# 
# ggplot(both0_plot2, aes(ACTIVITY_START_TS, meas)) +
#   geom_point() +
#   facet_grid(ANALYTE_NAME ~ Station.Name)

#Dealing with non-detects
unique(both0$VALUE_QUALIFER)

both1 <- both0 %>%
  mutate(no_info = case_when(is.na(ORG_RESULT_edited) & is.na(VALUE_QUALIFER) ~ "no info",
                             TRUE ~ "some info")) %>%
  filter(no_info=="some info")

#write.csv(both1, "both 2023-12-07.csv", row.names=FALSE)

both1 <- both1 %>%
  filter(is.na(VALUE_QUALIFER) | (VALUE_QUALIFER!="O" & VALUE_QUALIFER!="G" & VALUE_QUALIFER!="IG" & VALUE_QUALIFER!="JG" & VALUE_QUALIFER!="GJ" & 
                                    VALUE_QUALIFER!="IJG" & VALUE_QUALIFER!="YG" & VALUE_QUALIFER!="YJG" & VALUE_QUALIFER!="AG" & 
                                    VALUE_QUALIFER!="QG" & VALUE_QUALIFER!="GYQ" & VALUE_QUALIFER!="IGQ" & VALUE_QUALIFER!="IJGQ" & 
                                    VALUE_QUALIFER!="GQ" & VALUE_QUALIFER!="JGQ" & VALUE_QUALIFER!="IQG" & VALUE_QUALIFER!="GJQ" & 
                                    VALUE_QUALIFER!="JGI" & VALUE_QUALIFER!="IGY" & VALUE_QUALIFER!="GI" & VALUE_QUALIFER!="JIG" &
                                    VALUE_QUALIFER!="IGJ" & VALUE_QUALIFER!="IQJG" & VALUE_QUALIFER!="GJYQ" & 
                                    VALUE_QUALIFER!="YIG" & VALUE_QUALIFER!="IJYG" & VALUE_QUALIFER!="IYG" & VALUE_QUALIFER!="YIJG")) %>%
  filter(ORG_RESULT_edited!="*Not Reported" & ORG_RESULT_edited!="Not Reported") #%>%
#filter((ANALYTE_NAME!="TOCL" & ANALYTE_NAME!="KjeldahlNitrogen" & ANALYTE_NAME!="TotalPhosphorus") | (MON_LOC_NAME!="Lower Tampa Bay Middle Tampa Bay" & MON_LOC_NAME!="Old Tampa Bay" & MON_LOC_NAME!="Hillsborough Bay")) #some EPC estuary locations had ">" next to very low numbers of total phos, kjel nit, and TOCL...seems incorrect, so eliminated those site/analyte combinations

both2 <- both1 %>%
  mutate(detect_status = case_when((str_detect(VALUE_QUALIFER, "Z") | str_detect(VALUE_QUALIFER, "L")) & 
                                     !(str_detect(VALUE_QUALIFER, "U") | str_detect(VALUE_QUALIFER, "u") | str_detect(VALUE_QUALIFER, "T") | str_detect(VALUE_QUALIFER, "K")) ~ "above UDL",
                                   (str_detect(VALUE_QUALIFER, "U") | str_detect(VALUE_QUALIFER, "u") | str_detect(VALUE_QUALIFER, "T") | str_detect(VALUE_QUALIFER, "K")) & 
                                     !(str_detect(VALUE_QUALIFER, "Z") | str_detect(VALUE_QUALIFER, "L")) ~ "below MDL",
                                   (ORG_RESULT_edited=="*Non-detect" | ORG_RESULT_edited=="*Non-Detect" | ORG_RESULT_edited==as.character("0")) ~ "below MDL",
                                   TRUE ~ "needs eval")) %>%
  mutate(detect_status = case_when(detect_status=="needs eval" & (VALUE_QUALIFER=="J" | VALUE_QUALIFER=="I" | VALUE_QUALIFER=="QJ" | 
                                                                    VALUE_QUALIFER=="S" | VALUE_QUALIFER=="Q" | VALUE_QUALIFER=="IJ" | VALUE_QUALIFER=="Y" | 
                                                                    VALUE_QUALIFER=="YJ" | VALUE_QUALIFER=="IYJ" | VALUE_QUALIFER=="IY" | VALUE_QUALIFER=="JI" | 
                                                                    VALUE_QUALIFER=="IYJ" | VALUE_QUALIFER=="YJ" | VALUE_QUALIFER=="IQ" | VALUE_QUALIFER=="IV" | 
                                                                    VALUE_QUALIFER=="IJI" | VALUE_QUALIFER=="JIY" | VALUE_QUALIFER=="YV" | VALUE_QUALIFER=="JV" | 
                                                                    VALUE_QUALIFER=="JQI" | VALUE_QUALIFER=="QJI" | VALUE_QUALIFER=="YQ" | VALUE_QUALIFER=="JQ" | 
                                                                    VALUE_QUALIFER=="QY" | VALUE_QUALIFER=="V" | VALUE_QUALIFER=="VJ" | VALUE_QUALIFER=="JY" | 
                                                                    VALUE_QUALIFER=="IQY" | VALUE_QUALIFER=="IJIJ" | VALUE_QUALIFER=="IJQ" | VALUE_QUALIFER=="QV" | 
                                                                    VALUE_QUALIFER=="QI" | VALUE_QUALIFER=="YI" | VALUE_QUALIFER=="IJY" | VALUE_QUALIFER=="SJ" | 
                                                                    VALUE_QUALIFER=="IQJ" | VALUE_QUALIFER=="QIJ" | VALUE_QUALIFER=="i" | VALUE_QUALIFER=="A" | 
                                                                    VALUE_QUALIFER=="JA" | VALUE_QUALIFER=="AJ" | VALUE_QUALIFER=="AI" | VALUE_QUALIFER=="IA" | 
                                                                    VALUE_QUALIFER=="AIJ" | VALUE_QUALIFER=="AJI" | VALUE_QUALIFER=="AQ" | VALUE_QUALIFER=="AY" | 
                                                                    VALUE_QUALIFER=="AQV" | VALUE_QUALIFER=="AV" | VALUE_QUALIFER=="AJQ" | VALUE_QUALIFER=="IJR" | 
                                                                    VALUE_QUALIFER=="IR" | VALUE_QUALIFER=="JR" | VALUE_QUALIFER=="SR" | VALUE_QUALIFER=="AJR" | 
                                                                    VALUE_QUALIFER=="IAR" | VALUE_QUALIFER=="AR" | VALUE_QUALIFER=="BI" | VALUE_QUALIFER=="BIJ" | 
                                                                    VALUE_QUALIFER=="YBI" | VALUE_QUALIFER=="BIY" | VALUE_QUALIFER=="BZI" | VALUE_QUALIFER=="ZI" | 
                                                                    VALUE_QUALIFER=="BIV" | VALUE_QUALIFER=="BIYJ" | VALUE_QUALIFER=="IB" | VALUE_QUALIFER=="B.I" | 
                                                                    VALUE_QUALIFER=="D" | VALUE_QUALIFER=="R" | VALUE_QUALIFER=="C" | VALUE_QUALIFER=="CJ" |
                                                                    VALUE_QUALIFER=="B" | VALUE_QUALIFER=="BJ" | VALUE_QUALIFER=="BQ" | VALUE_QUALIFER=="BV" | VALUE_QUALIFER=="BY" | VALUE_QUALIFER=="BR" | VALUE_QUALIFER=="YB" | VALUE_QUALIFER=="BVJ" | VALUE_QUALIFER=="JB" | VALUE_QUALIFER=="BJV" | VALUE_QUALIFER=="JYQ" | VALUE_QUALIFER=="ij") ~ "fine",
                                   TRUE ~ detect_status)) %>%
  mutate(detect_status = case_when(detect_status=="needs eval" & non_num1==">" ~ "above UDL", #EPC
                                   detect_status=="needs eval" & non_num2=="<" ~ "below MDL",
                                   TRUE ~ detect_status)) #EPC
unique(both2$detect_status)
unique(both2$VALUE_QUALIFER)
unique(both2$ORG_MDL)
unique(both2$ORG_RESULT_edited)
unique(both2$ORG_RESULT_edited[is.na(as.numeric(both2$ORG_RESULT_edited))])
unique(both2$ORG_MDL[is.na(as.numeric(both2$ORG_MDL))])

both3 <- both2 %>%
  filter(ORG_RESULT_edited!="*Present >QL" & ORG_RESULT_edited!="*Present") %>%
  mutate(detect_status = case_when(ORG_RESULT_edited=="*Non-detect" ~ "below MDL",
                                   ORG_RESULT_edited=="*Non-detect" ~ "below MDL",
                                   TRUE ~ detect_status)) %>%
  mutate(detect_status = case_when(detect_status=="needs eval" & !is.na(ORG_MDL) & as.numeric(ORG_RESULT_edited)<as.numeric(ORG_MDL) ~ "below MDL",
                                   detect_status=="needs eval" & !is.na(ORG_MDL) & as.numeric(ORG_RESULT_edited)>=as.numeric(ORG_MDL) ~ "fine",
                                   detect_status=="needs eval" & is.na(VALUE_QUALIFER) ~ "fine",
                                   TRUE ~ detect_status))

unique(both3$VALUE_QUALIFER[both3$detect_status=="needs eval"])

# both3_summ1 <- both3 %>%
#   group_by(MON_LOC_NAME, ANALYTE_NAME) %>%
#   summarise(total = n())
# both3_summ2 <- both3 %>%
#   group_by(MON_LOC_NAME, ANALYTE_NAME, detect_status) %>%
#   summarise(n = n()) %>%
#   left_join(both3_summ1) %>%
#   ungroup() %>%
#   mutate(percent = n/total)

#unique(both3_summ2$ANALYTE_NAME[both3_summ2$detect_status=="above UDL" & both3_summ2$percent>.1])
#unique(both3$ANALYTE_NAME[both3$ORG_RESULT_edited=="*Present" | both3$ORG_RESULT_edited=="*Present >QL"])
#For sanity, I am removing these analytes

both4 <- both3 %>%
  filter(ANALYTE_NAME!="Depth, Secchi Disk Depth" & ANALYTE_NAME!="Secchi disk depth" & 
           ANALYTE_NAME!="Fecal Coliform" & ANALYTE_NAME!="Enterococci (CFU)" & 
           ANALYTE_NAME!="Escherichia coli" & ANALYTE_NAME!="Escherichia coli (CFU)" & 
           ANALYTE_NAME!="Enterococcus Group Bacteria" & ANALYTE_NAME!="FecalColiform" & 
           ANALYTE_NAME!="EColiorm" & ANALYTE_NAME!="Enterococci" & 
           ANALYTE_NAME!="Total Coliform" & ANALYTE_NAME!="Coliform Fecal (CFU)" & 
           ANALYTE_NAME!="Fecal Streptococcus Group Bacteria" & ANALYTE_NAME!="EColiform" &
           ANALYTE_NAME!="Escherichia coli (MPN)")

both5 <- both4 %>%
  filter(ORG_RESULT_edited!="*Present" & ORG_RESULT_edited!="*Present >QL") %>% #probably mis-recorded
  mutate(ORG_RESULT_edited = case_when(ORG_RESULT_edited=="-11.24" | ORG_RESULT_edited=="-0.68" | ORG_RESULT_edited=="-0.31" ~ "0",
                                       ORG_RESULT_edited=="*Non-detect" | ORG_RESULT_edited=="*Non-Detect" ~ "0",
                                       TRUE ~ ORG_RESULT_edited)) 

both6 <- both5 %>%
  mutate(ORG_RESULT_edited = as.numeric(ORG_RESULT_edited),
         ORG_MDL_edited = as.numeric(ORG_MDL)) %>%
  mutate(less_than_MDL = case_when(ORG_RESULT_edited<ORG_MDL_edited ~ "less than MDL",
                                   ORG_RESULT_edited==ORG_MDL_edited ~ "equal to MDL",
                                   ORG_RESULT_edited>ORG_MDL_edited ~ "greater than MDL",
                                   is.na(ORG_MDL_edited) ~ "MDL missing",
                                   TRUE ~ NA_character_)) %>%
  mutate(detect_status = case_when(detect_status=="below MDL" & less_than_MDL=="greater than MDL" ~ "fine",
                                   TRUE ~ detect_status))

#dealing with missing MDLs
both6_summ1 <- both6 %>%
  filter(detect_status=="fine" | detect_status=="above UDL") %>%
  group_by(Latitude, Longitude, ANALYTE_NAME) %>%
  summarise(lowest_det_val = min(ORG_RESULT_edited, na.rm=TRUE))

both6_summ2 <- both6 %>%
  filter(!is.na(ORG_MDL)) %>%
  group_by(Latitude, Longitude, ANALYTE_NAME, ORG_MDL) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(ORG_MDL = as.numeric(ORG_MDL))

both6_summ3 <- both6_summ2 %>%
  group_by(Latitude, Longitude, ANALYTE_NAME) %>%
  summarise(lowest_MDL = min(ORG_MDL),
            mode_MDL = max(n))

both6_summ4 <- both6_summ2 %>%
  left_join(both6_summ3, by=c("Latitude", "Longitude", "ANALYTE_NAME")) %>%
  mutate(mode = case_when(n==mode_MDL ~ "yes",
                          TRUE ~ "no")) %>%
  filter(mode=="yes") %>%
  select(-mode, -mode_MDL) %>%
  group_by(Latitude, Longitude, ANALYTE_NAME) %>%
  summarise(lowest_MDL = mean(lowest_MDL, na.rm=TRUE),
            mean_mode_MDL = mean(ORG_MDL, na.rm=TRUE))

both6_summ5 <- both6_summ1 %>%
  left_join(both6_summ4)

both7 <- both6 %>%
  left_join(both6_summ5) %>%
  mutate(ORG_MDL_edited = case_when(detect_status=="below MDL" & less_than_MDL=="MDL missing" & 
                                      !is.na(mean_mode_MDL) ~ mean_mode_MDL,
                                    detect_status=="below MDL" & less_than_MDL=="MDL missing" &
                                      is.na(mean_mode_MDL) & !is.na(lowest_MDL) ~ lowest_MDL,
                                    detect_status=="below MDL" & less_than_MDL=="MDL missing" &
                                      is.na(mean_mode_MDL) & is.na(lowest_MDL) & !is.na(lowest_det_val) ~ lowest_det_val,
                                    TRUE ~ ORG_MDL_edited)) %>%
  # mutate(ORG_MDL_edited = case_when(is.na(ORG_MDL_edited) ~ ORG_RESULT_edited,
  #                                   TRUE ~ ORG_MDL_edited)) %>%
  filter(is.na(ORG_MDL_edited) | ORG_MDL_edited!=0)

#pick sites (make sure 33 unique sites)
sites <- read.csv("01_Data/Sites.csv")
sites <- sites %>%
  mutate(Latitude = round(as.numeric(Latitude), 5),
         Longitude = round(as.numeric(Longitude), 5))

both8 <- both7 %>%
  filter(Latitude %in% sites$Latitude & Longitude %in% sites$Longitude)

both9 <- both8 %>%
  left_join(sites, by=c("Latitude", "Longitude")) %>%
  mutate(combo = paste0(Latitude, " ", Longitude, sep=""))

unique(both9$combo)

#pick analytes pass 1
av_analytes <- both9 %>%
   group_by(ANALYTE_NAME) %>%
   summarise(total_meas = n(),
             n_loc = length(unique(MON_LOC_NAME)))
 av_analytes <- as.data.frame(av_analytes)
 write.csv(av_analytes, "01_Data/Analytes unedited.csv", row.names=FALSE)
#will now limit dataset to analytes with at least 3 sites and 3 measurements, unless the analyte belongs in the same category as a more-measured analyte (due to spelling)

unique(both9$ANALYTE_NAME)
#setdiff(unique(both9$ANALYTE_NAME), analytes$ANALYTE_NAME)

analytes <- read.csv("01_Data/Analytes edited.csv")
analytes <- analytes %>%
  filter(!is.na(ANALYTE_GROUP2) & ANALYTE_GROUP2!="")
both10 <- both9 %>%
  filter(ANALYTE_NAME %in% analytes$ANALYTE_NAME) %>%
  left_join(analytes, by="ANALYTE_NAME")

#checking units
units <- both11 %>%
  group_by(ANALYTE_GROUP2, unit) %>%
  summarise(n = n()) %>%
  ungroup()

unique(analytes$ANALYTE_GROUP2)

both11 <- both10 %>%
  mutate(SOURCE = case_when(ALT_DEPTH=="DEP" ~ "DEP",
                            TRUE ~ "EPC")) %>%
  select(ACTIVITY_START_TS, MON_LOC_NAME, Station.Name, Name, USGS, desc,
         Latitude, Longitude, ACTIVITY_DEPTH, ACTIVITY_DEPTH_UNIT, ALT_DEPTH,
         ANALYTE_NAME, ANALYTE_GROUP1, ANALYTE_GROUP2, ORG_RESULT_edited, 
         ORG_RESULT_UNIT, ORG_MDL_edited, ORG_DETECTION_UNIT, detect_status, SOURCE) %>%
  mutate(unit = case_when(!is.na(ORG_RESULT_UNIT) & !is.na(ORG_DETECTION_UNIT) & ORG_RESULT_UNIT==ORG_DETECTION_UNIT ~ ORG_RESULT_UNIT,
                          !is.na(ORG_RESULT_UNIT) &  is.na(ORG_DETECTION_UNIT) ~ ORG_RESULT_UNIT,
                          is.na(ORG_RESULT_UNIT) & !is.na(ORG_DETECTION_UNIT) ~ ORG_DETECTION_UNIT,
                          !is.na(ORG_RESULT_UNIT) & !is.na(ORG_DETECTION_UNIT) & ORG_RESULT_UNIT!=ORG_DETECTION_UNIT ~ "check",
                          TRUE ~ "check")) %>%
  mutate(unit = case_when(unit=="ug/l" ~ "ug/L",
                          unit=="mg/l" ~ "mg/L",
                          unit=="umho/cm" ~ "uS/cm",
                          unit=="mg/m3" ~ "ug/L",
                          unit=="PSS" ~ "ppth",
                          unit=="mg/kg" ~ "mg/L",
                          TRUE ~ unit)) %>%
  mutate(unit = case_when((ANALYTE_GROUP2=="Ammonia" | ANALYTE_GROUP2=="Calcium" | ANALYTE_GROUP2=="Chloride" | ANALYTE_GROUP2=="Dissolved oxygen" | ANALYTE_GROUP2=="Fluoride" | ANALYTE_GROUP2=="Magnesium" | ANALYTE_GROUP2=="Orthophosphate as P" | ANALYTE_GROUP2=="Potassium" | ANALYTE_GROUP2=="Sodium" | ANALYTE_GROUP2=="Total dissolved solids" | ANALYTE_GROUP2=="Total nitrogen as N" | ANALYTE_GROUP2=="Total organic carbon" | ANALYTE_GROUP2=="Total phosphorus as P" | ANALYTE_GROUP2=="Nitrate-nitrite as N" | ANALYTE_GROUP2=="Organic nitrogen") & unit=="EPC" ~ "mg/L",
                          (ANALYTE_GROUP2=="Arsenic" | ANALYTE_GROUP2=="Cadmium" | ANALYTE_GROUP2=="Chlorophyll" | ANALYTE_GROUP2=="Chromium" | ANALYTE_GROUP2=="Copper" | ANALYTE_GROUP2=="Iron" | ANALYTE_GROUP2=="Lead" | ANALYTE_GROUP2=="Manganese" | ANALYTE_GROUP2=="Nickel") & unit=="EPC" ~ "ug/L",
                          TRUE ~ unit)) %>%
  mutate(ORG_RESULT_edited = case_when(unit=="mg/L" & (ANALYTE_GROUP2=="Iron" | ANALYTE_GROUP2=="Manganese") ~ ORG_RESULT_edited*(10^3),
                                       #unit=="ft" ~ ORG_RESULT_edited/3.281,
                                       #unit=="mS/cm" ~ ORG_RESULT_edited*(10^3),
                                       #unit=="ug/kg" ~ ORG_RESULT_edited*(10^3),
                                       TRUE ~ ORG_RESULT_edited)) %>%
  mutate(unit = case_when(unit=="mg/L" & (ANALYTE_GROUP2=="Iron" | ANALYTE_GROUP2=="Manganese") ~ "ug/L",
                          ANALYTE_GROUP2=="pH" ~ "SU",
                          ANALYTE_GROUP2=="Turbidity" ~ "NTU",
                          ANALYTE_GROUP2=="True color" | ANALYTE_GROUP2=="Color" ~ "PCU",
                          ANALYTE_GROUP2=="DO" ~ "mg/L",
                          ANALYTE_GROUP2=="Phosphorus" ~ "mg/L",
                          ANALYTE_GROUP2=="Nitrogen" ~ "mg/L",
                          ANALYTE_GROUP2=="Temperature" ~ "deg C",
                          ANALYTE_GROUP2=="Salinity" ~ "ppth",
                          ANALYTE_GROUP2=="Specific conductance" ~ "uS/cm",
                          ANALYTE_GROUP2=="Sulfate" ~ "mg/L",
                          ANALYTE_GROUP2=="TOC" ~ "mg/L",
                          #unit=="ft" ~ "m",
                          #unit=="mS/cm" ~ "uS/cm",
                          #unit=="ug/kg" ~ "ng/L",
                          TRUE ~ unit))

unique(both11$detect_status)
both11$ANALYTE_GROUP2[is.na(both11$ORG_MDL_edited) & both11$detect_status=="below MDL"]

both12 <- both11 %>%
  mutate(ACTIVITY_DEPTH = case_when(ACTIVITY_DEPTH_UNIT=="ft" ~ as.character(as.numeric(ACTIVITY_DEPTH)/3.28084),
                                    TRUE ~ ACTIVITY_DEPTH),
         ACTIVITY_DEPTH_UNIT = case_when(ACTIVITY_DEPTH_UNIT=="ft" ~ "m",
                                         ACTIVITY_DEPTH_UNIT=="EPC" ~ "m",
                                         TRUE ~ "m")) %>%
  mutate(site_analyte = paste(ANALYTE_GROUP2, " ", Name, sep=""))

unique(both12$site_analyte)
check <- both12 %>%
  filter(ANALYTE_GROUP2=="Iron")

i="DO Alafia R at Bell Shoals"
sub <- both12 %>%
  filter(ACTIVITY_START_TS>=as.POSIXct("2015-01-01 00:00:00"))
for (i in unique(sub$site_analyte)) {
    sub1 <- sub %>%
      filter(site_analyte==i)
    ggplot(sub1, aes(ACTIVITY_START_TS, ORG_RESULT_edited, color=ANALYTE_NAME)) +
      geom_point() +
      facet_wrap(~Station.Name) +
      theme(legend.position="bottom")
    ggsave(paste("01_Data/Check orig data/", i, ".png", sep=""), width=15, height=12)
}

sub2 <- sub %>%
  filter(ANALYTE_GROUP2=="Nitrogen")
for (i in unique(sub2$site_analyte)) {
  sub3 <- sub2 %>%
    filter(site_analyte==i)
  ggplot(sub3, aes(ACTIVITY_START_TS, ORG_RESULT_edited, color=ANALYTE_NAME)) +
  geom_point() +
  facet_grid(ANALYTE_NAME~Station.Name) +
  theme(legend.position="bottom")
  ggsave(paste("01_Data/Check orig data/", i, ".png", sep=""), width=15, height=16)
}

unique(both12$Station.Name)

# i="Alkalinity"
# for (i in unique(both11$ANALYTE_GROUP)) {
#   use <- filter(both11, ANALYTE_GROUP==i)
#   ggplot(use, aes(ACTIVITY_START_TS, ORG_RESULT_edited, shape=ANALYTE_NAME, color=ACTIVITY_DEPTH)) +
#     geom_point() +
#     ggtitle(i) +
#     facet_wrap(~Name, scales="free_y", ncol=2) +
#     theme(legend.position="right")
#   ggsave(paste0("Figs raw data with metadata/", i, ".png"), width=15, height=12)
# }

write.csv(both12, "Data sites filtered but not depths.csv", row.names=FALSE) #move to Used
