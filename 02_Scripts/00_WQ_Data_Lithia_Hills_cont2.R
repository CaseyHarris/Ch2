#install.packages(c("dataRetrieval", "EGRET"))

library(EGRET)
library(tidyverse)

data <- read.csv("01_Data/Data ready.csv")
#dates <- read.csv("Pre-EGRET data/Dates.csv")
data <- data %>%
  mutate(dataset_name = paste0(ANALYTE_GROUP2, " ", Name, sep=""))
# dates <- dates %>%
#   filter(ANALYTE_GROUP!="Total nitrogen as N") %>%
#   mutate(dataset_name = paste0(ANALYTE_GROUP, " ", Name, sep=""))
# unique(data$dataset_name)
# unique(dates$dataset_name)

# dates1 <- dates %>%
#   mutate(use_start = as.Date(WQ_SFN_start, format="%m/%d/%Y"),
#          use_end = as.Date(WQ_SFN_end, format="%m/%d/%Y"))

dataset_names <- unique(data$dataset_name)

data <- data %>%
  mutate(date = as.Date(date))

i = "Turbidity Alafia R at Lithia"
for (i in dataset_names) {
  
  print(i)
  
  # start_date <- dates1$use_start[dates1$dataset_name==i]
  # end_date <- dates1$use_end[dates1$dataset_name==i]
  # 
  # if (str_detect(i, " Div1")) {
  #   j = str_remove(i, " Div1")
  # } else if (str_detect(i, " Div2")) {
  #   j = str_remove(i, " Div2")
  # } else {
  #   j = i
  # }
  
  use <- data %>%
    filter(dataset_name==i) %>% #& 
             #date>=start_date & date<=end_date) %>%
    #mutate(date = paste0(month(date), "/", day(date), "/", year(date), sep="")) %>%
    select(date, remark, value)
  
  write.csv(use, paste0("01_Data/Redone wq and flow/", i, ".csv", sep=""), row.names=FALSE)
  fileName <- paste0(i, ".csv", sep="")
  filePath <- "01_Data/Redone wq and flow/"
  Sample <- readUserSample(filePath, fileName, separator=",")
  
  # use1 <- use %>%
  #   # mutate(date = as.Date(date),
  #   #        remark = case_when(is.na(ConcLow) ~ "<",
  #   #                           !is.na(ConcLow) ~ "")) %>%
  #   # mutate(value = case_when(remark=="<" ~ MDL_edit_new,
  #   #                          remark=="" ~ ConcHigh)) %>%
  #   mutate(remark = "",
  #          value = value) %>%
  #   select(date, remark, value)
  
  write.csv(Sample, paste0("01_Data/Redone wq and flow/", i, ".csv", sep=""), row.names=FALSE)
  
  # ggplot(use1, aes(date, value)) +
  #   geom_point() +
  #   ggtitle(dataset_names[i])
  # 
  # ggsave(paste0("Figs3/", dataset_names[i], ".png", sep=""), width=8, height=4)
}


# site_colors <- c("#999999", "#f781bf", "#e41a1c", "#377eb8", "#ff7f00", "#000000", "#984ea3", "#4daf4a", "#a65628")
