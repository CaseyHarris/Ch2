library(rmatio)
library(readxl)
library(tidyverse)

m <- read.mat("Data/Jason_flow/INTB_streamflow_vgrids.mat")
summary(m)

Alafia <- m$Alafia
Hills <- m$Hills

chang_dets <- read_excel("Notes/Vgrids_realization_list.xlsx", sheet="List_inputs")

chang_nums <- read_excel("Notes/Vgrids_realization_list.xlsx", sheet="Numbering", skip=1)
chang_nums <- chang_nums %>%
  select(Scenario, "100") #selects only the "business as usual" realizations
list_chang_nums <- chang_nums$"100"

dat <- Hills
dat_name <- "Hillsborough"
chang_data <- data.frame()
i=100
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
  start_date <- chang_dets$"Start date"[chang_dets$"Realization number"==row]
  end_date <- chang_dets$"End date"[chang_dets$"Realization number"==row]
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
  
chang_data1 <- chang_data %>%
  mutate(scenario = gsub("'", "", scenario))

write.csv(chang_data1, "Data/Chang data business as usual.csv", row.names=FALSE)
