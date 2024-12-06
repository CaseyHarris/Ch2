#Tileplot plots

dat <- read.table("C:/Users/cshar/Desktop/Ch2_git/Ch2/03_Results/Tileplots.csv", header=TRUE)
unique(dat$title)

dat1 <- dat %>%
  mutate(pos_neg_use1 = case_when(median_diff1>0 ~ pos_diff1,
                                  median_diff1<=0 ~ neg_diff1),
         pos_neg_use2 = case_when(median_diff2>0 ~ pos_diff2,
                                  median_diff2<=0 ~ neg_diff2)) %>%
  select(title, model1, month_names, median_past, median_diff1, median_diff2, pos_neg_use1, pos_neg_use2) %>%
  mutate(month_names = ordered(as.factor(month_names), levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

dat2a <- dat1 %>%
  pivot_longer(cols=c(median_diff1, median_diff2), values_to="median_diff")
dat2a <- dat2a %>%
  mutate(name_fut = case_when(str_detect(name, "1") ~ 1,
                              str_detect(name, "2") ~ 2)) %>%
  select(-name, -pos_neg_use1, -pos_neg_use2)

dat2b <- dat1 %>%
  pivot_longer(cols=c(pos_neg_use1, pos_neg_use2), values_to="pos_neg")
dat2b <- dat2b %>%
  mutate(name_fut = case_when(str_detect(name, "1") ~ 1,
                              str_detect(name, "2") ~ 2)) %>%
  select(-name, -median_diff1, -median_diff2)

dat2 <- dat2a %>%
  left_join(dat2b, by=c("title", "model1", "month_names", "median_past", "name_fut")) %>%
  mutate(site = case_when(str_detect(title, "Alafia") ~ "Alafia River",
                          str_detect(title, "Hillsborough") ~ "Hillsborough River",
                          TRUE ~ NA),
         param = str_replace(title, " Alafia R at Lithia", ""),
         param = str_replace(param, " Hillsborough R at Morris Br", ""),
         model1 = ordered(as.factor(model1), levels=c("GFDL-CM3", "MRI-CGCM3", "MPI-ESM-LR", "NorESM1-M", "MIROC-ESM", "BCC-CSM", "BNU-ESM", "GFDL-ESM2G")),
         model1 = fct_rev(model1),
         name1 = case_when(name_fut==1 ~ "Future1",
                           name_fut==2 ~ "Future2"),
         units = case_when(param=="Fluoride" | param=="Nitrate-nitrite as N" | param=="Orthophosphate as P" | param=="Total organic carbon" | param=="Total phosphorus as P" ~ "mg/l",
                           param=="Turbidity" ~ "NTU",
                           param=="True color" ~ "PCU",
                           param=="Specific conductance" ~ "µS/cm at 25°C")) %>%
  mutate(pos_neg_show = case_when(pos_neg>=95 ~ "*",
                                  .default = NA_character_))

#Differences
i="Specific conductance"
j="Alafia River"
for (i in unique(dat2$param)) {
  for (j in unique(dat2$site)) {
  
  dat3 <- dat2 %>%
    filter(param==i & site==j)
  
  limits = c(min(dat3$median_diff), max(dat3$median_diff))
  breaks = c(min(dat3$median_diff), max(dat3$median_diff))
  labels = c(signif(min(dat3$median_diff), 2), signif(max(dat3$median_diff), 2))
  
    plot_ij <- ggplot(dat3, aes(month_names, model1, fill=median_diff)) +
      geom_tile() +
      geom_text(aes(label=pos_neg_show)) +
      scale_fill_gradient2(name=dat3$units[1], low="blue", high="green",
                           limits=limits,
                           breaks=breaks,
                           labels=labels) +
      ggtitle(i) +
      facet_wrap(name1~., ncol=1) +
      theme_minimal() +
      theme(plot.title=element_text(size=10), plot.subtitle=element_text(size=10),
            strip.text=element_text(size=10),
            axis.title=element_blank(), 
            axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
            axis.text.y=element_text(size=10),
            legend.position="bottom",
            legend.title=element_text(size=10, hjust=.5, vjust=10),
            legend.title.position="bottom",
            legend.text=element_text(size=10, vjust=2.5))
    
    plot_ij

  i_name = i#gsub("-", "", i)
  j_name = j#gsub("-", "", j)
  name <- gsub(" ", "", paste(i_name, j_name))
  assign(name, plot_ij)

  }
}

library(patchwork)
SpecificconductanceAlafiaRiver
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Facet titles.png", width=6, height=6, bg="white")

p1 <- SpecificconductanceAlafiaRiver +
  FluorideAlafiaRiver + 
  `Nitrate-nitriteasNAlafiaRiver` + 
  OrthophosphateasPAlafiaRiver + 
  plot_layout(ncol=4, nrow=1,
              axes="collect")

p2 <- TotalphosphorusasPAlafiaRiver +
  TotalorganiccarbonAlafiaRiver +
  TruecolorAlafiaRiver +
  TurbidityAlafiaRiver +
  plot_layout(ncol=4, nrow=1,
              axes="collect")

p1 / p2 +
  plot_annotation(title="Alafia River")
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Alafia params1-8.png", width=7.5, height=9.5, bg="white")

p1 <- SpecificconductanceHillsboroughRiver +
  FluorideHillsboroughRiver + 
  `Nitrate-nitriteasNHillsboroughRiver` + 
  OrthophosphateasPHillsboroughRiver + 
  plot_layout(ncol=4, nrow=1,
              axes="collect")

p2 <- TotalphosphorusasPHillsboroughRiver +
  TotalorganiccarbonHillsboroughRiver +
  TruecolorHillsboroughRiver +
  TurbidityHillsboroughRiver +
  plot_layout(ncol=4, nrow=1,
              axes="collect")

p1 / p2 +
  plot_annotation(title="Hillsborough River")
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Hillsborough params1-8.png", width=7.5, height=9.5, bg="white")

#Define meaningful changes
#more than 5% of nutrients
#relevant steps of sp cond, color
#asterisks are placed on results with more than a .95 probability of a meaningful change occurring (see table);



# #Future values
# dat4a <- dat2 %>%
#   mutate(median = median_past,
#          name1 = "Past",
#          percent_diff = NA,
#          large_change = NA) %>%
#   select(-median_past, -median_diff, -name_fut, -pos_neg, -pos_neg_show)
#   
# dat4b <- dat2 %>%
#   mutate(median = median_past + median_diff) %>%
#   mutate(percent_diff = median_diff/median_past) %>%
#   mutate(large_change = case_when(percent_diff>=.05 | percent_diff<=-.05 ~ "*",
#                                         .default = NA_character_)) %>%
#   select(-median_past, -median_diff, -name_fut, -pos_neg, -pos_neg_show)
# 
# dat4 <- dat4a %>%
#   bind_rows(dat4b) %>%
#   mutate(name1 = ordered(as.factor(name1), levels=c("Past", "Future1", "Future2")))
# 
# i="Specific conductance"
# j="Alafia River"
# for (i in unique(dat2$param)) {
#   for (j in unique(dat2$site)) {
#     
#     dat5 <- dat4 %>%
#       filter(param==i & site==j)
#     
#     limits = c(min(dat5$median), max(dat5$median))
#     breaks = c(min(dat5$median), max(dat5$median))
#     labels = c(signif(min(dat5$median), 2), signif(max(dat5$median), 2))
#     
#     plot_ij <- ggplot(dat5, aes(month_names, model1, fill=median, linewidth=large_change)) +
#       geom_tile(color="red") +
#       #geom_text(aes(label=large_change)) +
#       scale_linewidth_manual(values=c(.5, 0)) +
#       scale_fill_gradient(name=dat5$units[1], low="white", high="black",
#                            limits=limits,
#                            breaks=breaks,
#                            labels=labels) +
#       ggtitle(i) +
#       facet_wrap(name1~., ncol=1) +
#       theme_minimal() +
#       theme(plot.title=element_text(size=10), plot.subtitle=element_text(size=10),
#             strip.text=element_text(size=10),
#             axis.title=element_blank(), 
#             axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
#             axis.text.y=element_text(size=10),
#             legend.position="bottom",
#             legend.title=element_text(size=10, hjust=.5, vjust=10),
#             legend.title.position="bottom",
#             legend.text=element_text(size=10, vjust=2.5))
#     
#     plot_ij
#     
#     i_name = i#gsub("-", "", i)
#     j_name = j#gsub("-", "", j)
#     name <- gsub(" ", "", paste(i_name, j_name))
#     assign(name, plot_ij)
#     
#   }
# }
# 
# library(patchwork)
# SpecificconductanceAlafiaRiver
# ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Facet titles.png", width=6, height=6, bg="white")
# 
# p1 <- SpecificconductanceAlafiaRiver +
#   FluorideAlafiaRiver + 
#   `Nitrate-nitriteasNAlafiaRiver` + 
#   OrthophosphateasPAlafiaRiver + 
#   plot_layout(ncol=4, nrow=1,
#               axes="collect")
# 
# p2 <- TotalphosphorusasPAlafiaRiver +
#   TotalorganiccarbonAlafiaRiver +
#   TruecolorAlafiaRiver +
#   TurbidityAlafiaRiver +
#   plot_layout(ncol=4, nrow=1,
#               axes="collect")
# 
# p1 / p2 +
#   plot_annotation(title="Alafia River")
# ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Alafia params1-8.png", width=7.5, height=13.5, bg="white")
# 
# p1 <- SpecificconductanceHillsboroughRiver +
#   FluorideHillsboroughRiver + 
#   `Nitrate-nitriteasNHillsboroughRiver` + 
#   OrthophosphateasPHillsboroughRiver + 
#   plot_layout(ncol=4, nrow=1,
#               axes="collect")
# 
# p2 <- TotalphosphorusasPHillsboroughRiver +
#   TotalorganiccarbonHillsboroughRiver +
#   TruecolorHillsboroughRiver +
#   TurbidityHillsboroughRiver +
#   plot_layout(ncol=4, nrow=1,
#               axes="collect")
# 
# p1 / p2 +
#   plot_annotation(title="Hillsborough River")
# ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Hillsborough params1-8.png", width=7.5, height=13.5, bg="white")
# 
