#Cameron Ho
#YNP Food for the Masses
#creating time series plots to show the number of individuals across the use of the carcass


library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)
library(hms)
library(data.table)
library(AER)  #dispersiontest() for glm
library(MASS)  #glm.nb() negative binomial 

se <- function(x){sqrt(var(x) / length(x))}


# Setting up 98-01 wolf acquired carcasses ---------------------------------
#reading in data
wilmers_data <- read_xlsx("data/wilmers_scavenger_count.xlsx")


#subsetting to wolf kill or fresh scavenge
carcass_meta <- read.csv("data/wolf_project_carcass_data.csv", header = T)
wilmers_carcmeta <- read_xlsx("data/wilmers_carcass_site_data.xlsx")

wilmers_carcmeta <- merge(wilmers_carcmeta, carcass_meta, by.x = 6, by.y = 1)
wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS") |
                                       wilmers_carcmeta$KILL.TYPE %in% c("DEFINITE WOLF", "PROBABLE WOLF"),]

wilmers_data <- wilmers_data[wilmers_data$ObservationSetID %in% wilmers_carcmeta$ObservationSetID,]
mort <- unique(wilmers_data$ObservationSetID)


#creating a list to store all the finished combined plots
wilmers_high_count <- matrix(nrow = length(mort), ncol = 7, dimnames = list(c(mort), c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle", "Wolf", "Cougar")))

for(i in 1:length(mort)){
  #subsetting the data for the desired mort #
  mort_data <- wilmers_data[wilmers_data$ObservationSetID == mort[i],]
  days <- unique(mort_data$Date)
  
  
  
  
  ######################### RAVEN
  if("R" %in% mort_data$SpeciesID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$SpeciesID == "R",]
    
    #combining counts from different areas
    raven_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(raven_data[raven_data$Date == days[d],]) == 0){
        tmp_raven_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_raven_data$Time <- factor(tmp_raven_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_raven_data <- data.frame(Date = days[d],
                                     Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                     Count = NA)
      }
      
      tmp_times <- tmp_raven_data$Time
      for(t in 1:length(tmp_times)){
        tmp_raven_data[t,3] <- sum(raven_data[raven_data$Date == days[d] & raven_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      raven_count_list[[d]] <- tmp_raven_data
    }
    raven_count_list <- do.call(rbind, raven_count_list)
    
    wilmers_high_count[i,1] <- max(raven_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("M" %in% mort_data$SpeciesID){
    
    #subsetting magpie data
    magpie_data <- mort_data[mort_data$SpeciesID == "M",]
    
    #combining counts from different areas
    magpie_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(magpie_data[magpie_data$Date == days[d],]) == 0){
        tmp_magpie_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_magpie_data$Time <- factor(tmp_magpie_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_magpie_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_magpie_data$Time
      for(t in 1:length(tmp_times)){
        tmp_magpie_data[t,3] <- sum(magpie_data[magpie_data$Date == days[d] & magpie_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      magpie_count_list[[d]] <- tmp_magpie_data
    }
    magpie_count_list <- do.call(rbind, magpie_count_list)
    
    wilmers_high_count[i,2] <- max(magpie_count_list$Count)
  }
  
  
  
  ######################### COYOTE
  if("C" %in% mort_data$SpeciesID){
    
    #subsetting coyote data
    coyote_data <- mort_data[mort_data$SpeciesID == "C",]
    
    #combining counts from different areas
    coyote_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(coyote_data[coyote_data$Date == days[d],]) == 0){
        tmp_coyote_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_coyote_data$Time <- factor(tmp_coyote_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_coyote_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_coyote_data$Time
      for(t in 1:length(tmp_times)){
        tmp_coyote_data[t,3] <- sum(coyote_data[coyote_data$Date == days[d] & coyote_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      coyote_count_list[[d]] <- tmp_coyote_data
    }
    coyote_count_list <- do.call(rbind, coyote_count_list)
    
    wilmers_high_count[i,3] <- max(coyote_count_list$Count)
  }
  
  
  
  ######################### BALD EAGLE
  if("BE" %in% mort_data$SpeciesID){
    
    #subsetting bald eagle data
    baea_data <- mort_data[mort_data$SpeciesID == "BE",]
    
    #combining counts from different areas
    baea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(baea_data[baea_data$Date == days[d],]) == 0){
        tmp_baea_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_baea_data$Time <- factor(tmp_baea_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_baea_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_baea_data$Time
      for(t in 1:length(tmp_times)){
        tmp_baea_data[t,3] <- sum(baea_data[baea_data$Date == days[d] & baea_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      baea_count_list[[d]] <- tmp_baea_data
    }
    
    baea_count_list <- do.call(rbind, baea_count_list)
    
    wilmers_high_count[i,4] <- max(baea_count_list$Count)
  }
  
  
  
  ######################### GOLDEN EAGLE
  if("GE" %in% mort_data$SpeciesID){
    
    #subsetting golden eagle data
    goea_data <- mort_data[mort_data$SpeciesID == "GE",]
    
    #combining counts from different areas
    goea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(goea_data[goea_data$Date == days[d],]) == 0){
        tmp_goea_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_goea_data$Time <- factor(tmp_goea_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_goea_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_goea_data$Time
      for(t in 1:length(tmp_times)){
        tmp_goea_data[t,3] <- sum(goea_data[goea_data$Date == days[d] & goea_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      goea_count_list[[d]] <- tmp_goea_data
    }
    
    goea_count_list <- do.call(rbind, goea_count_list)
    
    wilmers_high_count[i,5] <- max(goea_count_list$Count)
  }
  
  
  
  ######################### WOLF
  if("W" %in% mort_data$SpeciesID){
    
    #subsetting wolf data
    wolf_data <- mort_data[mort_data$SpeciesID == "W",]
    
    #combining counts from different areas
    wolf_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(wolf_data[wolf_data$Date == days[d],]) == 0){
        tmp_wolf_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_wolf_data$Time <- factor(tmp_wolf_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_wolf_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_wolf_data$Time
      for(t in 1:length(tmp_times)){
        tmp_wolf_data[t,3] <- sum(wolf_data[wolf_data$Date == days[d] & wolf_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      wolf_count_list[[d]] <- tmp_wolf_data
    }
    
    wolf_count_list <- do.call(rbind, wolf_count_list)
    
    wilmers_high_count[i,6] <- max(wolf_count_list$Count)
  }
  
  
  
  ######################### COUGAR
  if("ML" %in% mort_data$SpeciesID){
    
    #subsetting cougar data
    cougar_data <- mort_data[mort_data$SpeciesID == "ML",]
    
    #combining counts from different areas
    cougar_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(cougar_data[cougar_data$Date == days[d],]) == 0){
        tmp_cougar_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_cougar_data$Time <- factor(tmp_cougar_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_cougar_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_cougar_data$Time
      for(t in 1:length(tmp_times)){
        tmp_cougar_data[t,3] <- sum(cougar_data[cougar_data$Date == days[d] & cougar_data$Time == tmp_times[t],]$NumberofAnimals)
      }
      cougar_count_list[[d]] <- tmp_cougar_data
    }
    
    cougar_count_list <- do.call(rbind, cougar_count_list)
    
    wilmers_high_count[i,7] <- max(cougar_count_list$Count)
  }
}

wilmers_high_count[is.na(wilmers_high_count)] <- 0
wilmers_high_count <- round(as.data.frame(wilmers_high_count), 1)

wilmers_high_count$Study <- "1998-01"


#creating average species counts
wilmers_count_average <- data.frame(Average = colMeans(wilmers_high_count[,1:5]),
                                    se = sapply(wilmers_high_count[,1:5], se),
                                    study = "A", species = colnames(wilmers_high_count[,1:5]))



# Setting up 21-23 wolf acquired carcasses ---------------------------------
source("Average Species Count.R")

high_count <- as.data.frame(high_count)
high_count$Kill <- rownames(high_count)
high_count$Study <- "2021-22"


#removing cougar and late scavenged carcasses
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
high_count <- high_count[-which(rownames(high_count) %in% cat_mort),]
high_count <- high_count[rownames(high_count) != "25-1" 
                         & rownames(high_count) != "21-181"
                         & rownames(high_count) != "22-113",]


#creating average species counts
high_count_average <- data.frame(Average = colMeans(high_count[,1:5]),
                                 se = sapply(high_count[,1:5], se),
                                 study = "B", species = colnames(high_count[,1:5]))

# Comparison -------------------------------------------------------------
comb_dataset <- rbind(wilmers_high_count, high_count[,c(1:7, 9)])


  ## t-test ----
  #using independent two sampled t-test, no carcass is in both sample group
  #using equal variance if var are less than 4:1
    #'raven
    t.test(wilmers_high_count[,1], high_count[,1], alternative = "two.sided", paired = F, var.equal = T, conf.int = T)
    #' variance equal (259 vs 602)
      var(wilmers_high_count[,1])
      var(high_count[,1])
    #' normally distributed (98-01)
      shapiro.test(wilmers_high_count[,1])
      shapiro.test(high_count[,1])
    

    #'magpie
    wilcox.test(wilmers_high_count[,2], high_count[,2], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (30 vs 65)
      var(wilmers_high_count[,2])
      var(high_count[,2])
    #' not normally distributed (both)
      shapiro.test(wilmers_high_count[,2])
      shapiro.test(high_count[,2])
    
    
    #'coyote
    wilcox.test(wilmers_high_count[,3], high_count[,3], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (8.9 vs 3)
      var(wilmers_high_count[,3])
      var(high_count[,3])
    #' not normally distributed (98-01)
      shapiro.test(wilmers_high_count[,3])
      shapiro.test(high_count[,3])
    
    
    #'bald eagle
    wilcox.test(wilmers_high_count[,4], high_count[,4], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (3.3 vs 1.7)
      var(wilmers_high_count[,4])
      var(high_count[,4])
    #' not normally distributed (98-01)
      shapiro.test(wilmers_high_count[,4])
      shapiro.test(high_count[,4])
    
    
    #'golden eagle
    wilcox.test(wilmers_high_count[,5], high_count[,5], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (4.9 vs 2.6)
      var(wilmers_high_count[,5])
      var(high_count[,5])
    #' not normally distributed (both)
      shapiro.test(wilmers_high_count[,5])
      shapiro.test(high_count[,5])




  ## GLM ----
  #' overdispersiontest(_glm)
  #' all models were overdispersed
  
    #raven
    raven_glm <- glm.nb(Raven ~ study, data = comb_dataset)
    summary(raven_glm)
    glm.nb(Raven ~ 1, data = comb_dataset)$aic
    #742.22 vs 748.3778 (null)
    
    
    #magpie
    magpie_glm <- glm.nb(Magpie ~ study, data = comb_dataset)
    summary(magpie_glm)
    glm.nb(Magpie ~ 1, data = comb_dataset)$aic
    #540.05 vs 540.5103 (null)
    
    
    #coyote
    coyote_glm <- glm.nb(Coyote ~ study, data = comb_dataset)
    summary(coyote_glm)
    glm.nb(Coyote ~ 1, data = comb_dataset)$aic
    #417.93 vs 415.9812 (null)
    
    
    #bald eagle
    baea_glm <- glm.nb(`Bald Eagle` ~ study, data = comb_dataset)
    summary(baea_glm)
    glm.nb(`Bald Eagle` ~ 1, data = comb_dataset)$aic
    #316.93 vs 318.4263 (null)
    
    
    #golden eagle
    goea_glm <- glm.nb(`Golden Eagle` ~ study, data = comb_dataset)
    summary(goea_glm)
    glm.nb(`Golden Eagle` ~ 1, data = comb_dataset)$aic
    #317.14 vs 315.2863 (null)


# Comparison Plots --------------------------------------------------------------
  #' raw data
  plot.data <- rbind(high_count_average, wilmers_count_average)
  
  ggplot(data = plot.data, aes(x = species, y = Average, fill = study)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Average-se, ymax = Average+se), width=.2,
                  position=position_dodge(.9)) + 
    scale_fill_discrete(name="Sample Period",
                        breaks=c("A", "B"),
                        labels=c("1998-2001", "2021-2022")) +
    xlab("")

  
  #' raw data boxplot
  cb <- c("#56B4E9", "#D55E00")
  boxplot_data <- melt(setDT(comb_dataset[,c(1:5, 8)]), id.vars = 6, measure.vars = 1:5)
  names(boxplot_data)[2:3] <- c("Species", "Count")
    #creating data frame to annotate each facet to show if it was significant or not
    ann_df <- data.frame(Species = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"),
                        label = c("*", "", "", "*", ""),
                        y_level = c(103.7, 30, 17, 10, 15))
    ann_df$Species <- factor(ann_df$Species, levels = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"))
  
  
  p <- ggplot(data = boxplot_data, aes(x = Study, y = Count)) + 
    geom_boxplot(fill = c(cb, cb, cb, cb, cb))  +
    geom_segment(data = ann_df, inherit.aes = F, aes(x = "1998-01", xend = "2021-22", y = y_level, yend = y_level)) +
    labs(y = "Maximum Concurrent Count", x = "Study Period") +
    facet_wrap(~Species, nrow = 1, strip.position = "bottom", scales = "free_y") +
    theme_classic() + 
    theme(text = element_text(size=14),              #increasing axis font size
          strip.text = element_text(size = 17))      #increasing facet_wrap heading font size

  
  p + geom_text(ann_df, mapping = aes(x = 1.5, y = y_level+y_level/130, label = label), size = 5)
  