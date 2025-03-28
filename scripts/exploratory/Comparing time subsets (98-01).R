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

#histogram of when counts were taken
hist(as.numeric(format(wilmers_data$Time, "%H")))


#subsetting to wolf kill or fresh scavenge
carcass_meta <- read_xlsx("data/wolf_project_carcass_data.xlsx")
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
    
    #subsetting raven data
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
  
  
  
  ######################### MAGPIE
  if("C" %in% mort_data$SpeciesID){
    
    #subsetting raven data
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
    
    #subsetting raven data
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
    
    #subsetting raven data
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
    
    #subsetting raven data
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
    
    #subsetting raven data
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

# Subsetted time data --------------------------------------------------------
subset_time <- function(time){
  #reading in data
  data <- wilmers_data
  data <- filter(data, hour(as.POSIXct(data$Time, format = "%H:%M")) %in% time)
  mort <- unique(data$ObservationSetID)
  
  #creating a list to store all the finished combined times
  sub_high_count <- matrix(nrow = length(mort), ncol = 7, dimnames = list(c(mort), c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle", "Wolf", "Cougar")))
  
  for(i in 1:length(mort)){
    #subsetting the data for the desired mort #
    mort_data <- data[data$ObservationSetID == mort[i],]
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
      
      sub_high_count[i,1] <- max(raven_count_list$Count)
    }
    
    
    
    ######################### MAGPIE
    if("M" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,2] <- max(magpie_count_list$Count)
    }
    
    
    
    ######################### MAGPIE
    if("C" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,3] <- max(coyote_count_list$Count)
    }
    
    
    
    ######################### BALD EAGLE
    if("BE" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,4] <- max(baea_count_list$Count)
    }
    
    
    
    ######################### GOLDEN EAGLE
    if("GE" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,5] <- max(goea_count_list$Count)
    }
    
    
    
    ######################### WOLF
    if("W" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,6] <- max(wolf_count_list$Count)
    }
    
    
    
    ######################### COUGAR
    if("ML" %in% mort_data$SpeciesID){
      
      #subsetting raven data
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
      
      sub_high_count[i,7] <- max(cougar_count_list$Count)
    }
  }
  
  sub_high_count[is.na(sub_high_count)] <- 0
  sub_high_count <- round(sub_high_count, 1)
  
  return(sub_high_count)
}

sub_high_count <- subset_time(time = c(10:13))


# Comparison of max concurrent count --------------------------------------------------------

##splitting the morts into two groups so that samples are independent
set.seed(992)
group <- sample(1:nrow(wilmers_high_count), nrow(wilmers_high_count)/2, replace = F)

high_count_samp <- wilmers_high_count[group,]
sub_high_count_samp <- sub_high_count[-group,]

## t-test ----
#using independent two sampled t-test, no carcass is in both sample group
#paired t-test doesn't allow for only partly duplicated sample group
#using equal variance if var are less than 4:1

#'raven
t.test(high_count_samp[,1], sub_high_count_samp[,1], alternative = "two.sided", paired = F, var.equal = T, conf.int = T)
    #' variance equal
      var(high_count_samp[,1])
      var(sub_high_count_samp[,1])
    #' normally distributed
      shapiro.test(high_count_samp[,1])
      shapiro.test(sub_high_count_samp[,1])
    
    
#'magpie
t.test(high_count_samp[,2], sub_high_count_samp[,2], alternative = "two.sided", paired = F, var.equal = T, conf.int = T)
    #' variance equal
      var(high_count_samp[,2])
      var(sub_high_count_samp[,2])
    #' not normally distributed
      shapiro.test(high_count_samp[,2])
      shapiro.test(sub_high_count_samp[,2])
    
    
#'coyote
wilcox.test(high_count_samp[,3], sub_high_count_samp[,3], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (3 vs 3.2)
      var(high_count_samp[,3])
      var(sub_high_count_samp[,3])
    #' normally distributed
      shapiro.test(high_count_samp[,3])
      shapiro.test(sub_high_count_samp[,3])
    
    
#'bald eagle
wilcox.test(high_count_samp[,4], sub_high_count_samp[,4], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal
      var(high_count_samp[,4])
      var(sub_high_count_samp[,4])
    #' not normally distributed
      shapiro.test(high_count_samp[,4])
      shapiro.test(sub_high_count_samp[,4])
    
    
#'golden eagle
wilcox.test(high_count_samp[,5], sub_high_count_samp[,5], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal
      var(high_count_samp[,5])
      var(sub_high_count_samp[,5])
    #' not normally distributed
      shapiro.test(high_count_samp[,5])
      shapiro.test(sub_high_count_samp[,5])



# Results -----------------------------------------------------------------

    ##Comparing between all times and morning (1-12) ## seed 739
        #' raven  p = 0.32   var equal, U-test
        #' magpie p = 0.67   var equal, t-test
        #' coyote p = 0.67   var not equal, U-test
        #' bald   p = 0.85   var equal, U-test
        #' golden p = 0.23   var equal, U-test



    ##Comparing between all times and afternoon (13-24) ## seed 147
        #' raven  p = 0.005  var equal, t-test ****
        #' magpie p = 0.09   var equal, t-test
        #' coyote p = 0.045  var equal, t-test ****
        #' bald   p = 0.26   var equal, U-test
        #' golden p = 0.98   var equal, U-test 



    ##Comparing between all times and early morning (1-9) ## seed 536
        #' raven  p = 0.044  var equal, t-test ****
        #' magpie p = 0.003  var equal, U-test ****
        #' coyote p = 0.45   var equal, U-test
        #' bald   p = 0.048  var equal, U-test ****
        #' golden p = 0.84   var equal, U-test         



    ##Comparing between all times and late morning (10-13) ## seed 992
        #' raven  p = 0.79   var equal, t-test
        #' magpie p = 0.53   var equal, t-test
        #' coyote p = 0.81   var equal, U-test
        #' bald   p = 1      var equal, U-test 
        #' golden p = 0.88   var equal, U-test 
