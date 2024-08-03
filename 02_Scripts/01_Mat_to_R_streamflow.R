library(rmatio)
library(readxl)
library(tidyverse)

m <- read.mat("01_Data/INTB_streamflow_vgrids.mat")
summary(m)

Alafia <- m$Alafia #Lithia Pinecrest
Hills <- m$Hills #Morris Br

chang_dets <- read_excel("01_Data/Vgrids_realization_list.xlsx", sheet="List_inputs")

chang_nums <- read_excel("01_Data/Vgrids_realization_list.xlsx", sheet="Numbering", skip=1)
chang_nums <- chang_nums %>%
  select(Scenario, "100") #selects only the "business as usual" realizations
list_chang_nums <- chang_nums$"100"

dat <- Alafia
#dat <- Hills
dat_name <- "Alafia"
#dat_name <- "Hillsborough"

chang_data <- data.frame()
i=653
for (i in list_chang_nums) {
  
  #What to call it
  scenario <- chang_nums$Scenario[chang_nums$"100"==i]
  
  #Data
  new_data <- as.data.frame(dat[[i]])
  colnames(new_data) <- "flow"
  new_data$scenario = scenario
  new_data <- new_data %>%
    select(scenario, flow)
  
  #Dates
  chang_nums$row <- seq(1:76)
  row <- chang_nums$row[chang_nums$"100"==i]
  start_date <- chang_dets$"Output_start"[chang_dets$"Realization number"==row]
  end_date <- chang_dets$"Output_end"[chang_dets$"Realization number"==row]
  new_data$date <- seq.Date(from=as.Date(start_date), to=as.Date(end_date), by="day")
  
  new_data <- new_data %>%
    select(scenario, date, flow) %>%
    mutate(site = dat_name,
           Chang_item = i,
           Realiz_num = row)
  
  chang_data <- chang_data %>%
    bind_rows(new_data)
  
  rm(new_data)
  rm(scenario)
  rm(row)
  rm(start_date)
  rm(end_date)

}
  
chang_data_Alafia <- chang_data %>%
  mutate(scenario = gsub("'", "", scenario))

# chang_data_Hills <- chang_data %>%
#   mutate(scenario = gsub("'", "", scenario))

chang_data_both <- chang_data_Alafia %>%
  bind_rows(chang_data_Hills)

write.csv(chang_data_both, "01_Data/Chang data business as usual.csv", row.names=FALSE)
