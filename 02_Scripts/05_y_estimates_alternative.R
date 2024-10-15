#I think this is ready to run
library(tidyverse)
library(data.table)
library(readxl)
library(foreach)
library(doParallel)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(!str_detect(wq_names, "Temperature|Organic nitrogen|Chloride"))
unique(wq_names$wq_names)

max_min <- read.csv("03_Results/max_min.csv")
#max_min <- read.csv("/blue/carpena/caseyharris/Ch2/max_min.csv")

cluster <- makeCluster(2)
registerDoParallel(cluster)

i=1
foreach(i = 1:length(wq_names$wq_names),
        .packages=c('tidyverse', 'data.table', 'readxl', 'data.table')) %dopar% {
          
          title_use <- gsub(".csv", "", wq_names$wq_names[i])
          min_use <- max_min$value[max_min$title==title_use & max_min$name=="min_flow_obs_wq_2010_2019"]
          max_use <- max_min$value[max_min$title==title_use & max_min$name=="max_flow_obs_wq_2010_2019"]
          
          mcmc_summary <- read.table(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC/", title_use, ".csv", sep=""), header=TRUE)
          #mcmc_summary <- read.table(paste0("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title_use, ".csv", sep=""), header=TRUE)
          cal_val <- read.table(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Cal_val/", title_use, ".csv", sep=""), header=TRUE)
          #cal_val <- read.table(paste0("/blue/carpena/caseyharris/Ch2/JAGS/Cal_val/", title_use, ".csv", sep=""), header=TRUE)
          
          cal_val <- cal_val %>%
            mutate(row_number = row_number())
          
          n_obs_s1 <- cal_val %>%
            filter(split==1)
          n_obs <- length(n_obs_s1$title)
          
          j=1
          for (j in 1:length(mcmc_summary$split)) {
            
            y_row <- data.frame()
            
            k=1
            k_min = cal_val$row_number[cal_val$Q_cfs_log_round==min_use & cal_val$split==1]
            k_max = cal_val$row_number[cal_val$Q_cfs_log_round==max_use & cal_val$split==1]
            for (k in 1:n_obs) {
              
              if (cal_val$Q_cfs_log_round[k]<min_use) {
                set.seed(j*k)
                y <- rgamma(1, mcmc_summary$r[j], mcmc_summary$r[j]/mcmc_summary[j,k_min+2])
              } else if (cal_val$Q_cfs_log_round[k]>max_use) {
                set.seed(j*k)
                y <- rgamma(1, mcmc_summary$r[j], mcmc_summary$r[j]/mcmc_summary[j,k_max+2])
              } else {
                set.seed(j*k)
                y <- rgamma(1, mcmc_summary$r[j], mcmc_summary$r[j]/mcmc_summary[j,k+2])
              }
              
              y_new <- data.frame("split"=mcmc_summary$split[j], 
                                  "row"=j, 
                                  "Q_cfs_log_round"=cal_val$Q_cfs_log_round[k], 
                                  "y"=round(y, 5))
              y_row <- y_row %>%
                bind_rows(y_new)
              
            }
            
            y_row <- as.data.table(y_row)
            class(y_row)
            
            if (j==1) {
              write.table(y_row, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), row.names=FALSE)
              #write.table(y_row, paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), row.names=FALSE)
            } else {
              write.table(y_row, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
              #write.table(y_row, paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), row.names=FALSE, col.names=FALSE, append=TRUE)
            }
          } #very slow, like 8 hours per water quality parameter...
        }
stopCluster(cl = cluster)

check <- read.table(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""), header=TRUE)
check <- read.table(paste("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""), header=TRUE)