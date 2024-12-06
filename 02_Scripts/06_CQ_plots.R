library(tidyverse)
library(data.table)

wq_names <- list.files("01_Data/Redone wq and flow")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(str_detect(wq_names, "Alkalinity|Color|Fluoride|Iron|Manganese|Nitrogen|TOC|Phosphorus|Turbidity"))
unique(wq_names$wq_names)

max_min <- read.csv("03_Results/max_min.csv")
#max_min <- read.csv("/blue/carpena/caseyharris/Ch2/max_min.csv")

units <- data.frame(param=c("True color", "Total organic carbon", "Turbidity", "Total nitrogen", "Total phosphorus", "Fluoride", "Alkalinity", "Iron", "Manganese"),
                    orig_param=c("Color", "TOC", "Turbidity", "Nitrogen", "Phosphorus", "Fluoride", "Alkalinity", "Iron", "Manganese"),
                    units=c("Meas. (PCU)", "Conc. (mg/L)", "Meas. (NTU)", "Conc. (mg/L as N)", "Conc. (mg/L as P)", "Conc. (mg/L)", "Conc. (mg/L)", "Conc. (µg/L)", "Conc. (µg/L)"))

y_out_save = data.frame()
i=7
for (i in 1:length(wq_names$wq_names)) {

  title_use <- gsub(".csv", "", wq_names$wq_names[i])
  min_use <- max_min$value[max_min$title==title_use & max_min$name=="min_flow_obs_wq_2013_2022"]
  max_use <- max_min$value[max_min$title==title_use & max_min$name=="max_flow_obs_wq_2013_2022"]

  cal_val <- fread(paste0("01_Data/CQ/", title_use, ".csv", sep=""))
  cal_val <- cal_val %>%
    mutate(row_number = row_number())

  mcmc_summary <- fread(paste("03_Results/MCMC/", title_use, ".csv", sep=""))
  #mcmc_summary <- fread(paste0("/blue/carpena/caseyharris/Ch2/JAGS/MCMC/", title_use, ".csv", sep=""))
  mcmc_trans <- mcmc_summary %>%
    select(-r) %>%
    t()
  mcmc_trans <- as.data.frame(mcmc_trans)
  mcmc <- cal_val %>%
    bind_cols(mcmc_trans) %>%
    arrange(row_number)
  
  mcmc_dups <- mcmc %>% #there are duplicate flows in mcmc when more than 1 water quality sample is taken at the same flow
    select(Q_cfs_log_round)
  mcmc$dups <- duplicated(mcmc_dups)
  
  mcmc1 <- mcmc %>%
    filter(dups==FALSE) %>% #removes extra copy of duplicates
    select(-dups)
  
  j=2
  k=2
  use_min <- mcmc1[mcmc1$Q_cfs_log_round==min_use]
  use_max <- mcmc1[mcmc1$Q_cfs_log_round==max_use]
  mcmc2 <- mcmc1
  for (j in 1:length(mcmc1$title)) {
    if (mcmc1$Q_cfs_log_round[j]<min_use) {
      use_j <- use_min %>%
        mutate(Q_cfs_log_round = mcmc1$Q_cfs_log_round[j])
      mcmc2 <- mcmc2 %>%
        filter(Q_cfs_log_round!=mcmc1$Q_cfs_log_round[j]) %>%
        bind_rows(use_j)
    }
    if (mcmc1$Q_cfs_log_round[j]>max_use) {
      use_j <- use_max %>%
        mutate(Q_cfs_log_round = mcmc1$Q_cfs_log_round[j])
      mcmc2 <- mcmc2 %>%
        filter(Q_cfs_log_round!=mcmc1$Q_cfs_log_round[j]) %>%
        bind_rows(use_j)
    }
  } #takes several minutes
  
  write.csv(mcmc2, paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC_intermed/", title_use, ".csv", sep=""))

}

for (i in 1:length(wq_names$wq_names)) {
  
  mcmc2 <- read.csv(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/MCMC_intermed/", title_use, ".csv", sep=""))
  mcmc_long <- mcmc2 %>%
    pivot_longer(cols=starts_with("V"), names_to="sim", values_to="mu")
  
  y <- fread(file=paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""))
  #y <- fread(paste0("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""))

  #Plots
  #mean expected value line, 95% credible interval of expected value shaded
  #95% credible interval of y estimates shaded
  #sample of y estimates as points
  #measured wq values as points
  
  mcmc_long1 <- mcmc_long %>%
    #filter(Q_cfs_log_round<=max_use & Q_cfs_log_round>=min_use) %>%
    group_by(Q_cfs_log_round) %>%
    summarise(mu_mean = mean(mu),
              mu_median = median(mu),
              mu_05 = quantile(mu, .05),
              mu_95 = quantile(mu, .95))
  
  y_long <- y %>%
    pivot_longer(cols=starts_with("V"), names_to="sim", values_to="y") %>%
    group_by(Q_cfs_log_round) %>%
    summarise(y_mean = mean(y),
              y_median = median(y),
              y_05 = quantile(y, .05),
              y_95 = quantile(y, .95))
  
  y_out <- y %>%
    group_by(model, per) %>%
    mutate(tot = n()) %>%
    ungroup() %>%
    group_by(model, per, abv_bel, tot) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(prop = n/tot) %>%
    filter(abv_bel=="fine") %>%
    mutate(not_fine = 1 - prop)
  
  y_out_new <- data.frame("title"=title_use, "model"=y_out$model, "per"=y_out$per, "not_fine"=y_out$not_fine)
  y_out_save <- y_out_save %>%
    bind_rows(y_out_new)
  
  wq_orig <- mcmc %>%
    filter(!is.na(wq_orig)) %>%
    select(Q_cfs_log_round, wq_orig) %>%
    unique()
  
  orig_param = gsub(" Alafia R at Lithia", "", title_use)
  orig_param = gsub(" Hillsborough R at Morris Br", "", orig_param)
  unit = units$units[units$orig_param==orig_param]
  param = units$param[units$orig_param==orig_param]
  min_label_min = round(exp(max_min$value[max_min$title==title_use & max_min$name=="min_flow_1989_2100"]), 0)
  min_label_max = round(exp(max_min$value[max_min$title==title_use & max_min$name=="min_flow_obs_wq_2013_2022"]), 0)
  max_label_min = signif(exp(max_min$value[max_min$title==title_use & max_min$name=="max_flow_obs_wq_2013_2022"]), 2)
  max_label_max = signif(exp(max_min$value[max_min$title==title_use & max_min$name=="max_flow_1989_2100"]), 2)
  
  plot_i <- ggplot(y_long, aes(exp(Q_cfs_log_round))) +
    geom_ribbon(aes(ymin=y_05, ymax=y_95), fill="lightgray", alpha=.5) +
    geom_ribbon(data=filter(y_long, Q_cfs_log_round>=min_use & Q_cfs_log_round<=max_use), aes(ymin=y_05, ymax=y_95), fill="lightgray") +
    geom_ribbon(data=mcmc_long1, aes(ymin=mu_05, ymax=mu_95), fill="darkgray", alpha=.5) +
    geom_ribbon(data=filter(mcmc_long1, Q_cfs_log_round>=min_use & Q_cfs_log_round<=max_use), aes(ymin=mu_05, ymax=mu_95), fill="darkgray", alpha=.5) +
    geom_line(data=mcmc_long1, aes(exp(Q_cfs_log_round), mu_median), linetype="dashed") +
    geom_line(data=filter(mcmc_long1, Q_cfs_log_round>=min_use & Q_cfs_log_round<=max_use), aes(exp(Q_cfs_log_round), mu_median)) +
    geom_point(data=wq_orig, aes(y=wq_orig), shape=1, size=.5) +
    scale_x_log10(limits=c(10, 7000), 
                   breaks=c(10, 100, 1000, 5000), 
                   labels=c(paste("to ", min_label_min, sep=""), 100, 1000, 
                            paste("to ", max_label_max, sep=""))) +
    ggtitle(param) +
    xlab("Streamflow (cfs)") +
    ylab(unit) +
    theme_minimal() +
    theme(plot.title=element_text(size=10),
          axis.title=element_text(size=10), 
          # legend.position="bottom",
          # legend.title=element_text(size=10, hjust=.5, vjust=10),
          # legend.title.position="bottom",
          # legend.text=element_text(size=10, vjust=2.5),
          axis.text=element_text(size=10))
  plot_i
  #ggsave("CQ with legend.png")
  
  name <- gsub(" ", "", title_use)
  assign(name, plot_i)
  
  print(i)
}

write.csv(y_out_save, "03_Results/out of range.csv", row.names=FALSE)

#eventually make plots with min/max obs labeled along with a middle number
ColorAlafiaRatLithia <- ColorAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000),
               breaks=c(10, 100, 1000))
TurbidityAlafiaRatLithia <- TurbidityAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
TOCAlafiaRatLithia <- TOCAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
FluorideAlafiaRatLithia <- FluorideAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
PhosphorusAlafiaRatLithia <- PhosphorusAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
NitrogenAlafiaRatLithia <- NitrogenAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
AlkalinityAlafiaRatLithia <- AlkalinityAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
IronAlafiaRatLithia <- IronAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
ManganeseAlafiaRatLithia <- ManganeseAlafiaRatLithia +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))

ColorHillsboroughRatMorrisBr <- ColorHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000),
                breaks=c(10, 100, 1000))
TurbidityHillsboroughRatMorrisBr <- TurbidityHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
TOCHillsboroughRatMorrisBr <- TOCHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
FluorideHillsboroughRatMorrisBr <- FluorideHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
PhosphorusHillsboroughRatMorrisBr <- PhosphorusHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))
NitrogenHillsboroughRatMorrisBr <- NitrogenHillsboroughRatMorrisBr +
  scale_x_log10(limits=c(10, 7000), 
                breaks=c(10, 100, 1000))

library(patchwork)
p1 <- ColorAlafiaRatLithia + TOCAlafiaRatLithia + TurbidityAlafiaRatLithia + 
  plot_layout(ncol=3, nrow=1, axes="collect_x")

p2 <- NitrogenAlafiaRatLithia + PhosphorusAlafiaRatLithia + FluorideAlafiaRatLithia +
  plot_layout(ncol=3, nrow=1, axes="collect_x")

p3 <- AlkalinityAlafiaRatLithia + IronAlafiaRatLithia + ManganeseAlafiaRatLithia +
  plot_layout(ncol=3, nrow=1, axes="collect_x")

p1 / p2 / p3 +
  plot_annotation(title="Alafia River", 
                  theme=theme(plot.title = element_text(size = 10)))
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/CQ/Alafia params1-9.png", 
       width=6.5, height=7.5, bg="white")


p1 <- ColorHillsboroughRatMorrisBr + TOCHillsboroughRatMorrisBr + TurbidityHillsboroughRatMorrisBr + 
  plot_layout(ncol=3, nrow=1, axes="collect_x")

p2 <- NitrogenHillsboroughRatMorrisBr + PhosphorusHillsboroughRatMorrisBr + FluorideHillsboroughRatMorrisBr +
  plot_layout(ncol=3, nrow=1, axes="collect_x")

p1 / p2 +
  plot_annotation(title="Hillsborough River",
                  theme=theme(plot.title = element_text(size = 10)))
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/CQ/Hillsborough params1-6.png", 
       width=6.5, height=5, bg="white")


oor <- read.csv("03_Results/out of range.csv")
oor <- oor %>%
  filter(title=="Fluoride Alafia R at Lithia" | title=="Fluoride Hillsborough R at Morris Br") %>%
  select(-title) %>%
  unique()
