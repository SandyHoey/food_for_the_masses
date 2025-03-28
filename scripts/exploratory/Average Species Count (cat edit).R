#Cameron Ho
#YNP Food for the Masses
#creating time series plots to show the number of individuals across the use of the carcass
#edited to split high count calculation of cat kills by most recent predator (wolf/cat)


library(lubridate)
library(dplyr)
library(hms)
library(data.table)

#reading in data
data <- read.csv("All Obs.csv", header=T)

#subsetting only cat kills
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
data <- data[data$Kill.. %in% cat_mort,]
# data <- data[!data$Consumption.Stage..pile.1. %like% 4  | as.character(data$Consumption.Stage..pile.1.) == "2/3/4",]
mort <- unique(data$Kill..)
mort_rowname <- c(paste0(mort, "_cat"), paste0(mort,"_wolf"))
data$datetime <- paste(mdy(data$Date), data$Time)
uniq_dt <- unique(data$datetime)


#creating a list to store all the finished combined plots
cat_high_count <- matrix(nrow = length(mort_rowname), ncol = 7, dimnames = list(c(mort_rowname), c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle", "Wolf", "Cougar")))


#loop to split data by most recent predator (default is cat)
cat_cat <- data.frame()
cat_wolf <- data.frame()

for(dt in 1:length(unique(data$datetime))){
  tmp_dt <- data[data$datetime == uniq_dt[dt],]
  
  #pulling out the latest predator
  #doing wolves after cat so that if wolf take precedence if both are present
  if("MOUNTAIN LION" %in% tmp_dt$Species.ID){last_pred <- tmp_dt[tmp_dt$Species.ID == "MOUNTAIN LION",]}
  if("WOLF" %in% tmp_dt$Species.ID){last_pred <- tmp_dt[tmp_dt$Species.ID == "WOLF",]}
  
  #splitting data by last_pred
  if(dt == 1){if(last_pred[1,]$Species.ID == "WOLF"){cat_wolf <- tmp_dt}else(cat_cat <- tmp_dt)}
  if(dt != 1 & last_pred[1,]$Species.ID == "WOLF"){cat_wolf <- rbind(cat_wolf, tmp_dt)}
  if(dt != 1 & last_pred[1,]$Species.ID == "MOUNTAIN LION"){cat_cat <- rbind(cat_cat, tmp_dt)}
}



#### Loop to combine all areas for cats ----
for(i in 1:length(mort)){
  #subsetting the data for the desired mort #
  mort_data <- cat_cat[cat_cat$Kill.. == mort[i],]
  days <- unique(mort_data$Date)
  
  
  
  
  ######################### RAVEN
  if("RAVEN" %in% mort_data$Species.ID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$Species.ID == "RAVEN",]
    
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
        tmp_raven_data[t,3] <- sum(raven_data[raven_data$Date == days[d] & raven_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      raven_count_list[[d]] <- tmp_raven_data
    }
    raven_count_list <- do.call(rbind, raven_count_list)
    
    cat_high_count[i,1] <- max(raven_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("MAGPIE" %in% mort_data$Species.ID){
    
    #subsetting magpie data
    magpie_data <- mort_data[mort_data$Species.ID == "MAGPIE",]
    
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
        tmp_magpie_data[t,3] <- sum(magpie_data[magpie_data$Date == days[d] & magpie_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      magpie_count_list[[d]] <- tmp_magpie_data
    }
    magpie_count_list <- do.call(rbind, magpie_count_list)
    
    cat_high_count[i,2] <- max(magpie_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("COYOTE" %in% mort_data$Species.ID){
    
    #subsetting coyote data
    coyote_data <- mort_data[mort_data$Species.ID == "COYOTE",]
    
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
        tmp_coyote_data[t,3] <- sum(coyote_data[coyote_data$Date == days[d] & coyote_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      coyote_count_list[[d]] <- tmp_coyote_data
    }
    coyote_count_list <- do.call(rbind, coyote_count_list)
    
    cat_high_count[i,3] <- max(coyote_count_list$Count)
  }
  
  
  
  ######################### BALD EAGLE
  if("BALD EAGLE" %in% mort_data$Species.ID){
    
    #subsetting baea data
    baea_data <- mort_data[mort_data$Species.ID == "BALD EAGLE",]
    
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
        tmp_baea_data[t,3] <- sum(baea_data[baea_data$Date == days[d] & baea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      baea_count_list[[d]] <- tmp_baea_data
    }
    
   baea_count_list <- do.call(rbind, baea_count_list)
    
    cat_high_count[i,4] <- max(baea_count_list$Count)
  }
  
  
  
  ######################### GOLDEN EAGLE
  if("GOLDEN EAGLE" %in% mort_data$Species.ID){
    
    #subsetting goea data
    goea_data <- mort_data[mort_data$Species.ID == "GOLDEN EAGLE",]
    
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
        tmp_goea_data[t,3] <- sum(goea_data[goea_data$Date == days[d] & goea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      goea_count_list[[d]] <- tmp_goea_data
    }
    
    goea_count_list <- do.call(rbind, goea_count_list)
    
    cat_high_count[i,5] <- max(goea_count_list$Count)
  }
  
  
  
  ######################### WOLF
  if("WOLF" %in% mort_data$Species.ID){
    
    #subsetting wolf data
    wolf_data <- mort_data[mort_data$Species.ID == "WOLF",]
    
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
        tmp_wolf_data[t,3] <- sum(wolf_data[wolf_data$Date == days[d] & wolf_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      wolf_count_list[[d]] <- tmp_wolf_data
    }
    
    wolf_count_list <- do.call(rbind, wolf_count_list)
    
    cat_high_count[i,6] <- max(wolf_count_list$Count)
  }
  
  
  
  ######################### COUGAR
  if("MOUNTAIN LION" %in% mort_data$Species.ID){
    
    #subsetting raven data
    cougar_data <- mort_data[mort_data$Species.ID == "MOUNTAIN LION",]
    
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
        tmp_cougar_data[t,3] <- sum(cougar_data[cougar_data$Date == days[d] & cougar_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      cougar_count_list[[d]] <- tmp_cougar_data
    }
    
    cougar_count_list <- do.call(rbind, cougar_count_list)
    
    cat_high_count[i,7] <- max(cougar_count_list$Count)
  }
}



#### Loop to combine all areas for wolves ----
for(i in 1:length(mort)){
  #subsetting the data for the desired mort #
  mort_data <- cat_wolf[cat_wolf$Kill.. == mort[i],]
  days <- unique(mort_data$Date)
  
  
  
  
  ######################### RAVEN
  if("RAVEN" %in% mort_data$Species.ID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$Species.ID == "RAVEN",]
    
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
        tmp_raven_data[t,3] <- sum(raven_data[raven_data$Date == days[d] & raven_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      raven_count_list[[d]] <- tmp_raven_data
    }
    raven_count_list <- do.call(rbind, raven_count_list)
    
    cat_high_count[i+length(mort),1] <- max(raven_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("MAGPIE" %in% mort_data$Species.ID){
    
    #subsetting magpie data
    magpie_data <- mort_data[mort_data$Species.ID == "MAGPIE",]
    
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
        tmp_magpie_data[t,3] <- sum(magpie_data[magpie_data$Date == days[d] & magpie_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      magpie_count_list[[d]] <- tmp_magpie_data
    }
    magpie_count_list <- do.call(rbind, magpie_count_list)
    
    cat_high_count[i+length(mort),2] <- max(magpie_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("COYOTE" %in% mort_data$Species.ID){
    
    #subsetting coyote data
    coyote_data <- mort_data[mort_data$Species.ID == "COYOTE",]
    
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
        tmp_coyote_data[t,3] <- sum(coyote_data[coyote_data$Date == days[d] & coyote_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      coyote_count_list[[d]] <- tmp_coyote_data
    }
    coyote_count_list <- do.call(rbind, coyote_count_list)
    
    cat_high_count[i+length(mort),3] <- max(coyote_count_list$Count)
  }
  
  
  
  ######################### BALD EAGLE
  if("BALD EAGLE" %in% mort_data$Species.ID){
    
    #subsetting baea data
    baea_data <- mort_data[mort_data$Species.ID == "BALD EAGLE",]
    
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
        tmp_baea_data[t,3] <- sum(baea_data[baea_data$Date == days[d] & baea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      baea_count_list[[d]] <- tmp_baea_data
    }
    
    baea_count_list <- do.call(rbind, baea_count_list)
    
    cat_high_count[i+length(mort),4] <- max(baea_count_list$Count)
  }
  
  
  
  ######################### GOLDEN EAGLE
  if("GOLDEN EAGLE" %in% mort_data$Species.ID){
    
    #subsetting goea data
    goea_data <- mort_data[mort_data$Species.ID == "GOLDEN EAGLE",]
    
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
        tmp_goea_data[t,3] <- sum(goea_data[goea_data$Date == days[d] & goea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      goea_count_list[[d]] <- tmp_goea_data
    }
    
    goea_count_list <- do.call(rbind, goea_count_list)
    
    cat_high_count[i+length(mort),5] <- max(goea_count_list$Count)
  }
  
  
  
  ######################### WOLF
  if("WOLF" %in% mort_data$Species.ID){
    
    #subsetting wolf data
    wolf_data <- mort_data[mort_data$Species.ID == "WOLF",]
    
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
        tmp_wolf_data[t,3] <- sum(wolf_data[wolf_data$Date == days[d] & wolf_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      wolf_count_list[[d]] <- tmp_wolf_data
    }
    
    wolf_count_list <- do.call(rbind, wolf_count_list)
    
    cat_high_count[i+length(mort),6] <- max(wolf_count_list$Count)
  }
  
  
  
  ######################### COUGAR
  if("MOUNTAIN LION" %in% mort_data$Species.ID){
    
    #subsetting raven data
    cougar_data <- mort_data[mort_data$Species.ID == "MOUNTAIN LION",]
    
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
        tmp_cougar_data[t,3] <- sum(cougar_data[cougar_data$Date == days[d] & cougar_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      cougar_count_list[[d]] <- tmp_cougar_data
    }
    
    cougar_count_list <- do.call(rbind, cougar_count_list)
    
    cat_high_count[i+length(mort),7] <- max(cougar_count_list$Count)
  }
}



# -------------------------------------------------------------------
cat_high_count[is.na(cat_high_count)] <- 0
cat_high_count <- round(cat_high_count, 1)

#remove rows that sum to 0 (wolves were never present)
cat_high_count <- cat_high_count[-which(rowSums(cat_high_count) == 0),]

cat_mort <- rownames(cat_high_count[cat_high_count[,7] == 0,])

colMeans(cat_high_count[cat_high_count[,7] == 0, 1:6])
