library(tidyverse)
library(data.table)
library(readxl)

wq_names <- list.files("01_Data/Orig wq and flow/WQ")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(!str_detect(wq_names, "Temperature|Organic nitrogen|Chloride"))
unique(wq_names$wq_names)

#max_min <- read.csv("03_Results/max_min.csv")
#max_min <- read.csv("/blue/carpena/caseyharris/Ch2/max_min.csv")

i=1
title_use <- gsub(".csv", "", wq_names$wq_names[i])
exceed <- read.csv(paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/Exceedance/", title_use, ".csv", sep=""))

# threshold <- dist_df1 %>%
#     filter(y>round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) & y<(round(median(dist_df1$y[dist_df1$Chang_item==0]), 2) + .1)) %>%
#     group_by(Chang_item, model, time) %>%
#     summarise(thresh=mean(per_exceed))
  
ggplot(exceed, aes(wq_level, mean_exceed*100, group=Chang_item, color=model)) +
    ##geom_hline(yintercept=10^(log10(8)), linetype="solid", color="#1A3693", linewidth=1) +
    #geom_hline(yintercept=median(dist_df1$y[dist_df1$Chang_item==0]), linetype="solid", linewidth=1) +
    ##geom_segment(data=threshold, aes(x=thresh*100, y=10^(log10(1)), xend=thresh*100, yend=10^(log10(8)), group=mod, color=mod), linetype="dashed", linewidth=1) +
    #geom_segment(data=filter(threshold, Chang_item!=4), aes(x=thresh*100, y=-Inf, xend=thresh*100, yend=median(dist_df1$y[dist_df1$Chang_item==0]), group=model, color=model), linetype="dashed", linewidth=1) +
    geom_line(linewidth=1) +
    geom_errorbar(aes(ymin=low_exceed_95*100, ymax=high_exceed_95*100, group=Chang_item, color=model)) +
    geom_errorbar(aes(ymin=low_exceed_90*100, ymax=high_exceed_90*100, group=Chang_item, color=model)) +
    ##scale_x_continuous(limits=c(NA, .999*100), breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    #scale_x_continuous(breaks=c(5, 25, 50, 75, 95), labels=c(5, 25, 50, 75, 95)) +
    ##scale_y_continuous(limits=c(.1, 250)) + #specific to analyte
    #scale_color_manual(values=c("USGS"="black", "NLDAS-2_Hargreaves"="#7570b3", "GFDL_CM3_Hargreaves"="#1b9e77", "GFDL_ESM2G_Hargreaves"="#d95f02")) +
    xlab("Water quality (units)") +
    ylab("Percent exceedance (%)") +
    facet_wrap(~per, nrow=1) +
    theme_minimal() +
    theme(plot.background=element_rect(fill="white"),
          axis.text=element_text(size=10),
          axis.title=element_text(size=10),
          strip.text=element_text(size=10),
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          legend.position="none")
  ggsave(paste0("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Exceedance probabilities/", title_use, ".png", sep=""), width=6.5, height=3.5)