library(tidyverse)
library(readxl)
library(foreach)
library(doParallel)

wq_names <- list.files("01_Data/Redone wq and flow")
#wq_names <- list.files("/blue/carpena/caseyharris/Ch2/WQ")
wq_names <- data.frame(wq_names) %>%
  filter(str_detect(wq_names, "Lithia|Morris")) %>%
  filter(str_detect(wq_names, "Alkalinity|Color|Fluoride|Iron|Manganese|Nitrogen|TOC|Phosphorus|Turbidity"))
unique(wq_names$wq_names)

units <- data.frame(param=c("True color", "Total organic carbon", "Turbidity", "Total nitrogen", "Total phosphorus", "Fluoride", "Alkalinity", "Iron", "Manganese"),
                    orig_param=c("Color", "TOC", "Turbidity", "Nitrogen", "Phosphorus", "Fluoride", "Alkalinity", "Iron", "Manganese"),
                    units=c("Change (PCU)", "Change (mg/L)", "Change (NTU)", "Change (mg/L as N)", "Change (mg/L as P)", "Change (mg/L)", "Change (mg/L)", "Change (µg/L)", "Change (µg/L)"))

#stopCluster(cluster)
cluster <- makeCluster(2)
registerDoParallel(cluster)
i=15
median_diff_save <- data.frame(0)
#foreach(i = 1:length(wq_names$wq_names),
#        .packages=c('tidyverse', 'readxl')) %dopar% {
for (i in 1:length(wq_names$wq_names)) {

  title_use <- gsub(".csv", "", wq_names$wq_names[i])
  
  y <- fread(file=paste("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Results/y/", title_use, ".csv", sep=""))
  #y <- fread(paste0("/blue/carpena/caseyharris/Ch2/Results/y/", title_use, ".csv", sep=""))
  
  y1 <- y %>%
    mutate(date = as.Date(date),
           month = lubridate::month(date, label=TRUE, abbr=TRUE)) %>%
    pivot_longer(cols=starts_with("V"),) %>%
    group_by(wq_name, site, model, per, month, name) %>%
    summarise(median = median(value)) %>%
    pivot_wider(names_from=per, values_from=median)
  
  colnames(y1) = make.names(colnames(y1))
  
  y2 <- y1 %>%
    ungroup() %>%
    mutate(Fut1diff = X2030.2060 - X1989.2012,
           Fut2diff = X2070.2100 - X1989.2012) %>%
    mutate(Fut1pos = case_when(Fut1diff>0 ~ 1,
                               TRUE ~ 0),
           Fut1neg = case_when(Fut1diff<0 ~ 1,
                               TRUE ~ 0),
           Fut2pos = case_when(Fut2diff>0 ~ 1,
                               TRUE ~ 0),
           Fut2neg = case_when(Fut2diff<0 ~ 1,
                               TRUE ~ 0)) %>%
    group_by(wq_name, site, model, month) %>%
    summarise(Fut1diff_median = median(Fut1diff),
              Fut2diff_median = median(Fut2diff),
              Fut1likepos = sum(Fut1pos),
              Fut1likeneg = sum(Fut1neg),
              Fut2likepos = sum(Fut2pos),
              Fut2likeneg = sum(Fut2neg)) %>%
    mutate(Fut1like = case_when(Fut1likepos>=Fut1likeneg ~ Fut1likepos,
                                Fut1likeneg>Fut1likepos ~ Fut1likeneg),
           Fut2like = case_when(Fut2likepos>=Fut2likeneg ~ Fut2likepos,
                                Fut2likeneg>Fut2likepos ~ Fut2likeneg))
  
  y3 <- y2 %>%
    select(-Fut1likepos, -Fut1likeneg, -Fut2likepos, -Fut2likeneg) %>%
    mutate(Fut1med_use = case_when(Fut1like>=80 ~ Fut1diff_median,
                                TRUE ~ 0),
           Fut2med_use = case_when(Fut2like>80 ~ Fut2diff_median,
                                   TRUE ~ 0)) %>%
    mutate(model = gsub("_Hargreaves", "", model),
           model = gsub("_", "-", model),
           model = case_when(model=="bcc-csm" ~ "BCC-CSM",
                             TRUE ~ model)) %>%
    mutate(model = ordered(as.factor(model), levels=c("GFDL-CM3", "MRI-CGCM3", "MPI-ESM-LR", "NorESM1-M", "MIROC-ESM", "BCC-CSM", "BNU-ESM", "GFDL-ESM2G")),
           model = fct_rev(model))
  
  median_diff_save <- y3 %>%
    bind_rows(median_diff_save)
  
  unique(y2$model)
  unique(y3$model) 
  #TILE PLOTS

  min_diff <- min(c(min(y3$Fut1med_use[!is.na(y3$Fut1med_use)]), min(y3$Fut2med_use[!is.na(y3$Fut2med_use)])))
  max_diff <- max(c(max(y3$Fut1med_use[!is.na(y3$Fut1med_use)]), max(y3$Fut2med_use[!is.na(y3$Fut2med_use)])))
  
  orig_param = gsub(" Alafia R at Lithia", "", title_use)
  orig_param = gsub(" Hillsborough R at Morris Br", "", orig_param)
  unit = units$units[units$orig_param==orig_param]
  
  plot_i1 <- ggplot(y3, aes(month, model, fill=Fut1med_use)) +
    geom_tile() +
    #geom_text(aes(label=pos_diff1/100)) +
    scale_fill_gradient2(name=unit, low="#4dac26", high="#d01c8b",
                         limits=c(min_diff, max_diff),
                         breaks=c(min_diff, 0, max_diff),
                         labels=c(round(min_diff, 2), "0", round(max_diff, 2)),
                         na.value="white") +
    ggtitle(orig_param, subtitle="2030-2060 - retrospective") +
    #labs(fill=unit) +
    theme_minimal() +
    theme(axis.title=element_blank(), 
          legend.position="none",
          #legend.title=element_text(size=10), legend.text=element_text(size=10),
          axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
          axis.text.y=element_text(size=10),
          plot.title=element_text(size=10), plot.subtitle=element_text(size=10))
  
  plot_i2 <- ggplot(y3, aes(month, model, fill=Fut2med_use)) +
    geom_tile() +
    #geom_text(aes(label=pos_diff1/100)) +
    scale_fill_gradient2(name=unit, low="#4dac26", high="#d01c8b",
                         limits=c(min_diff, max_diff),
                         breaks=c(min_diff, 0, max_diff),
                         labels=c(round(min_diff, 2), "0", round(max_diff, 2)),
                         na.value="white") +
    ggtitle(orig_param, subtitle="2070-2100 - retrospective") +
    labs(fill=unit) +
    theme_minimal() +
    theme(axis.title=element_blank(), 
          legend.position="bottom",
          legend.title.position="top",
          legend.title=element_text(size=10), legend.text=element_text(size=10),
          axis.text.x=element_text(size=10, angle=90, vjust=.5, hjust=1), 
          axis.text.y=element_text(size=10),
          plot.title=element_blank(), plot.subtitle=element_text(size=10))
  
  name1 <- paste(gsub(" ", "", title_use), "1", sep="")
  name2 <- paste(gsub(" ", "", title_use), "2", sep="")
  assign(name1, plot_i1)
  assign(name2, plot_i2)
  
}

write.csv(median_diff_save, "03_Results/Median differences.csv", row.names=FALSE)

library(patchwork)
p1 <- ColorAlafiaRatLithia1 + TOCAlafiaRatLithia1 + TurbidityAlafiaRatLithia1 +
  ColorAlafiaRatLithia2 + TOCAlafiaRatLithia2 + TurbidityAlafiaRatLithia2 +
  plot_layout(ncol=3, nrow=2, axes="collect")
p1

p2 <- NitrogenAlafiaRatLithia1 + PhosphorusAlafiaRatLithia1 + FluorideAlafiaRatLithia1 +
  NitrogenAlafiaRatLithia2 + PhosphorusAlafiaRatLithia2 + FluorideAlafiaRatLithia2 +
  plot_layout(ncol=3, nrow=2, axes="collect")
p2

p3 <- AlkalinityAlafiaRatLithia1 + IronAlafiaRatLithia1 + ManganeseAlafiaRatLithia1 +
  AlkalinityAlafiaRatLithia2 + IronAlafiaRatLithia2 + ManganeseAlafiaRatLithia2 +
  plot_layout(ncol=3, nrow=2, axes="collect")
p3

p1 / p2 #+
  #plot_annotation(title="Alafia River")
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Alafia params1-6.png", width=6.5, height=8.5, bg="white")

p3
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Alafia params7-9.png", width=6.5, height=4.25, bg="white")

p1 <-  ColorHillsboroughRatMorrisBr1 + TOCHillsboroughRatMorrisBr1 + plot_spacer() +
  ColorHillsboroughRatMorrisBr2 + TOCHillsboroughRatMorrisBr2 + plot_spacer() +
  plot_layout(ncol=3, nrow=2, axes="collect")

p2 <- NitrogenHillsboroughRatMorrisBr1 + PhosphorusHillsboroughRatMorrisBr1 + FluorideHillsboroughRatMorrisBr1 +
  NitrogenHillsboroughRatMorrisBr2 + PhosphorusHillsboroughRatMorrisBr2 + FluorideHillsboroughRatMorrisBr2 +
  plot_layout(ncol=3, nrow=2, axes="collect")

p1 / p2 #+
  #plot_annotation(title="Hillsborough River")
ggsave("C:/Users/cshar/OneDrive - University of Florida/Online_diss_files/Ch2/Large files/Figures/Tileplots/Hillsborough params1-6.png", width=6.5, height=8.5, bg="white")

