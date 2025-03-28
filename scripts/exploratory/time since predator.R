#calculating the time since a cat or wolf was present within the FOV
#scavenger count is also within the FOV
#the idea is how does the presence of the predator impact the willingness of the scavengers
#to commit time to staying at the carcass

library(tidyverse)
library(readxl)


# Reading, Compiling, and organizing raw data ---------------------------------
#combining new and Wilmers scavenger counts

#reading in data
new_scav <- read.csv("data/new_scavenger_count.csv")
wilmers_scav <- read_xlsx("data/wilmers_scavenger_count.xlsx")


#restricting new_scav to definite, probable wolf kills and fresh scavenges
carcass_meta <- read.csv("data/wolf_project_carcass_data.csv", header = T)
new_carcmeta <- read_xlsx("data/new_carcass_site_data.xlsx")

new_carcmeta <- merge(new_carcmeta, carcass_meta, by.x = 1, by.y = 1)
new_carcmeta <- new_carcmeta[new_carcmeta$`Kill .` != "21-181" &
                               new_carcmeta$`Kill .` != "21-198",]


#merging new_scav with meta data to add
#' date of death
#' prey species and age
new_scav <- merge(new_scav, new_carcmeta[,c(1,8:10)], by.x = 2, by.y = 1)


#fixing date format for new_scav
new_scav$Date <- mdy(new_scav$Date)

#changing area shorthand in new data
new_scav[new_scav$Area == "AT CARCASS",]$Area <- "C"
new_scav[new_scav$Area == "CARCASS ARENA",]$Area <- "CA"
new_scav[new_scav$Area == "FIELD OF VIEW",]$Area <- "FOV"


#restricting wilmers_scav to 
#' wolf kills and fresh scavenges
#' those observed from before stage 3
wilmers_carcmeta <- read_xlsx("data/wilmers_carcass_site_data.xlsx")

wilmers_carcmeta <- merge(wilmers_carcmeta, carcass_meta, by.x = 6, by.y = 1)
wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS") |
                                       wilmers_carcmeta$COD %in% c("DEFINITE WOLF", "PROBABLE WOLF"),]
wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$`starting point` <= 3,]
wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$`Mortality Number` != "00-200" &
                                       wilmers_carcmeta$`Mortality Number` != "00-126" &
                                       wilmers_carcmeta$`Mortality Number` != "00-203",]

#merging wilmers_scav with metadata to 
#' add kill#
#' date of death
#' prey species
#' prey age
wilmers_scav <- merge(wilmers_scav, wilmers_carcmeta[,c(1,2,7,8,25)], by = "ObservationSetID")


#changing species identifier in Wilmer's data
wilmers_scav[wilmers_scav$SpeciesID == "R",]$SpeciesID <- "RAVEN"
wilmers_scav[wilmers_scav$SpeciesID == "M",]$SpeciesID <- "MAGPIE"
wilmers_scav[wilmers_scav$SpeciesID == "C",]$SpeciesID <- "COYOTE"
wilmers_scav[wilmers_scav$SpeciesID == "BE",]$SpeciesID <- "BALD EAGLE"
wilmers_scav[wilmers_scav$SpeciesID == "GE",]$SpeciesID <- "GOLDEN EAGLE"
wilmers_scav[wilmers_scav$SpeciesID == "F",]$SpeciesID <- "FOX"
wilmers_scav[wilmers_scav$SpeciesID == "W",]$SpeciesID <- "WOLF"


#fixing column names
names(new_scav)[names(new_scav) %in% colnames(new_scav[,c(1,3:7,19:21)])] <- c("Mort", "Date", "Time", "Area", "SpeciesID", "NumberofAnimals", "DOD", "Prey", "Age")
names(wilmers_scav)[names(wilmers_scav) %in% colnames(wilmers_scav[,c(13,5,16,14,15)])] <- c("Area", "Mort", "Prey", "Age", "DOD")


#extracting times from wilmers_scav
#fixing date format for wilmers_scav
wilmers_scav$Time <- format(wilmers_scav$Time, format = "%H:%M")
wilmers_scav$Date <- as.Date(wilmers_scav$Date)


#restricting data to winter months
new_scav <- new_scav[month(new_scav$Date) %in% c(11,12,1,2,3),]
wilmers_scav <- wilmers_scav[month(wilmers_scav$Date) %in% c(11,12,1,2,3),]


#combining wilmers_scav and new_scav into one dataframe
data <- rbind(new_scav[,c(1,3:7,19:21)], wilmers_scav[,c(13,3:7,16,14,15)])
#data <- new_scav[,c(1,3,4,6,7,18:20)]


# combining counts from area around the carcass -----------------------------------------------------

#restricting data to only species of interest
data <- subset(data, SpeciesID %in% c("RAVEN", "MAGPIE", "COYOTE", "BALD EAGLE",
                                      "GOLDEN EAGLE", "FOX", "WOLF", "MOUNTAIN LION"))

#creating a list to store all the finished combined plots
mort <- unique(data$Mort)
data_combine_area_list <- vector("list", length = length(mort))

data$DateTime <- paste(data$Date, data$Time)

for(i in 1:length(mort)){
  #subsetting the data for the desired mort #
  mort_data <- data[data$Mort == mort[i],]
  
  
  ######################### RAVEN
  if("RAVEN" %in% mort_data$SpeciesID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$SpeciesID == "RAVEN",]
    datetime <- unique(raven_data$DateTime)
    
    #combining counts from different areas
    raven_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "RAVEN", Count = NA)
    raven_count$Mort <- raven_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(raven_data, DateTime == datetime[d])
      
      raven_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], raven_count)
  }
  
  
  ######################### MAGPIE
  if("MAGPIE" %in% mort_data$SpeciesID){
    
    #subsetting magpie data
    magpie_data <- mort_data[mort_data$SpeciesID == "MAGPIE",]
    datetime <- unique(magpie_data$DateTime)
    
    #combining counts from different areas
    magpie_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "MAGPIE", Count = NA)
    magpie_count$Mort <- magpie_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(magpie_data, DateTime == datetime[d])
      
      magpie_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], magpie_count)
  }
  
  
  ######################### COYOTE
  if("COYOTE" %in% mort_data$SpeciesID){
    
    #subsetting coyote data
    coyote_data <- mort_data[mort_data$SpeciesID == "COYOTE",]
    datetime <- unique(coyote_data$DateTime)
    
    #combining counts from different areas
    coyote_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "COYOTE", Count = NA)
    coyote_count$Mort <- coyote_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(coyote_data, DateTime == datetime[d])
      
      coyote_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], coyote_count)
  }
  
  
  ######################### FOX
  if("FOX" %in% mort_data$SpeciesID){
    #subsetting raven data
    fox_data <- mort_data[mort_data$SpeciesID == "FOX",]
    datetime <- unique(fox_data$DateTime)
    
    #combining counts from different areas
    fox_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "FOX", Count = NA)
    fox_count$Mort <- fox_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(fox_data, DateTime == datetime[d])
      
      fox_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], fox_count)
  }
  
  
  ######################### BALD EAGLE
  if("BALD EAGLE" %in% mort_data$SpeciesID){
    
    #subsetting bald eagle data
    baea_data <- mort_data[mort_data$SpeciesID == "BALD EAGLE",]
    datetime <- unique(baea_data$DateTime)
    
    #combining counts from different areas
    baea_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "BALD EAGLE", Count = NA)
    baea_count$Mort <- baea_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(baea_data, DateTime == datetime[d])
      
      baea_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], baea_count)
  }
  
  
  
  ######################### GOLDEN EAGLE
  if("GOLDEN EAGLE" %in% mort_data$SpeciesID){
    
    #subsetting golden eagle data
    goea_data <- mort_data[mort_data$SpeciesID == "GOLDEN EAGLE",]
    datetime <- unique(goea_data$DateTime)
    
    #combining counts from different areas
    goea_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "GOLDEN EAGLE", Count = NA)
    goea_count$Mort <- goea_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(goea_data, DateTime == datetime[d])
      
      goea_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], goea_count)
  }
  
  
  
  ######################### WOLF
  if("WOLF" %in% mort_data$SpeciesID){
    
    #subsetting wolf data
    wolf_data <- mort_data[mort_data$SpeciesID == "WOLF",]
    datetime <- unique(wolf_data$DateTime)
    
    #combining counts from different areas
    wolf_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "WOLF", Count = NA)
    wolf_count$Mort <- wolf_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(wolf_data, DateTime == datetime[d])
      
      wolf_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], wolf_count)
  }
  
  
  
  ######################### COUGAR
  if("MOUNTAIN LION" %in% mort_data$SpeciesID){
    
    #subsetting cougar data
    cat_data <- mort_data[mort_data$SpeciesID == "MOUNTAIN LION",]
    datetime <- unique(cat_data$DateTime)
    
    #combining counts from different areas
    cat_count <- data.frame(Mort = NA, DateTime = datetime, SpeciesID = "MOUNTAIN LION", Count = NA)
    cat_count$Mort <- cat_data[1,"Mort"]
    
    for(d in 1:length(datetime)){
      tmp_datetime <- subset(cat_data, DateTime == datetime[d])
      
      cat_count[d, "Count"] <- sum(tmp_datetime$NumberofAnimals)
    }
    
    data_combine_area_list[[i]] <- rbind(data_combine_area_list[[i]], cat_count)
  }
}

#combining list of all Mort
#recombining with the original data to add back the kill information
#and changing column names
data_combine_area <- do.call("rbind", data_combine_area_list)
data_combine_area <- left_join(data_combine_area, carcass_meta[,c(1,6,8,21,23)], by = c("Mort" = "Kill.."))
names(data_combine_area) <- c("Mort", "DateTime", "SpeciesID", "Count", 
                              "Prey", "DOD", "Sex", "Age")
data_combine_area$DateTime <- ymd_hm(data_combine_area$DateTime)



#sorting by date -> time -> SpeciesID so that the predators are the first row in each timestep
data_combine_area <- arrange(data_combine_area, Mort, DateTime, 
                             match(SpeciesID, c("WOLF", "MOUNTAIN LION", "RAVEN", "MAGPIE", 
                                                "COYOTE", "BALD EAGLE", "GOLDEN EAGLE", "FOX")))



# time since last predator ------------------------------------------------

# loop that goes through each row for each kill
killnum <- unique(data_combine_area$Mort)


for(k in 1:length(killnum)){
  
  #subsetting to a single kill
  tmp_kill_data <- subset(data_combine_area, Mort == killnum[k])
  
  #resetting wolf and cat for each Mort
  MostRecentWolf <- NA
  MostRecentCat <- NA
  
  #going through each row of that kill
  for(j in 1:nrow(tmp_kill_data)){
    
    #selecting which image to compare
    tmp_row <- tmp_kill_data[j,]
    
    # if the image is a predator, mark it as the most recent sighting
    if(tmp_row$SpeciesID == "WOLF"){
      MostRecentWolf <- tmp_row
    }
    
    if(tmp_row$SpeciesID == "MOUNTAIN LION"){
      MostRecentCat <- tmp_row
    }
    
    # if the row is a scavenger
    # suppressing a warning created by is.na() when MostRecent... is a row and not NA
    suppressWarnings(if(tmp_row$Species != "WOLF" & tmp_row$Species != "MOUNTAIN LION"){
      
      # marking TimeSinceWolf as NA if there has not been a wolf detected yet
      # otherwise calculating time difference
      if(class(MostRecentWolf) != "data.frame"){
        tmp_kill_data[j, "TimeSinceWolf"] <- NA
      }else{
        tmp_kill_data[j, "TimeSinceWolf"] <- as.numeric(difftime(tmp_row$DateTime, MostRecentWolf$DateTime, units="mins"))
        
      }
      
      # marking TimeSinceCat as NA if there has not been a cougar detected yet
      # otherwise calculating time difference if not
      if(class(MostRecentCat) != "data.frame"){
        tmp_kill_data[j, "TimeSinceCat"] <- NA
      }else{
        tmp_kill_data[j, "TimeSinceCat"] <- as.numeric(difftime(tmp_row$DateTime, MostRecentCat$DateTime, units="mins"))
      }
    })
  }
  
  #creating a new dataframe with the time caluclation
  if(k == 1){
    data_time_since <- tmp_kill_data
  } else{
    data_time_since <- rbind(data_time_since, tmp_kill_data)
  }
}




# exploring data ----------------------------------------------------------
test <- subset(data_time_since, Mort == "07-317")
raven_time <- subset(data_time_since, SpeciesID == "RAVEN")

summary(raven_time)

test_glm <- glm(Count ~ TimeSinceWolf, 
    data = raven_time, family = "s")
summary(test_glm)

plot(Count ~ TimeSinceWolf, 
    data = subset(data_time_since, SpeciesID == "RAVEN"))
mean(raven_time$Count, na.rm=T)
var(raven_time$Count, na.rm=T) #super overdispersed

raven.glmm.nb <- glmmTMB::glmmTMB(Count ~ TimeSinceWolf + (1|Mort), data = raven_time)
summary(raven.glmm.nb)


