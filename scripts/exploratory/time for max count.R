#finding which day and time within that day would likely offer the highest 
#concurrent scavenger count 

library(lubridate)
library(readxl)
library(data.table)
library(plyr)
library(chron)



# Reading, Compiling, and organizing raw data ---------------------------------
  #reading in data
  new_data <- read.csv("data/new_scavenger_count.csv")
  wilmers_data <- as.data.frame(read_xlsx("data/wilmers_scavenger_count.xlsx"))
  
  
  #restricting new_data to definite, probable wolf kills and fresh scavenges
  carcass_meta <- read.csv("data/wolf_project_carcass_data.csv", header = T)
  new_carcmeta <- read_xlsx("data/new_scavenger_site_data.xlsx")
  
  new_carcmeta <- merge(new_carcmeta, carcass_meta, by.x = 1, by.y = 1)
  new_carcmeta <- new_carcmeta[new_carcmeta$KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS") |
                                 new_carcmeta$COD %like% "WOLF" |
                                 new_carcmeta$`Kill .` %in% c("22-036", "22-029", "22-021", "22-012", "22-004", "21-225"),]
  new_carcmeta <- new_carcmeta[new_carcmeta$`Kill .` != "21-181" &
                                 new_carcmeta$`Kill .` != "21-198",]
  
  
  #merging new_data with meta data to add
  #' date of death
  #' prey species and age
  new_data <- merge(new_data, new_carcmeta[,c(1,8:10)], by.x = 2, by.y = 1)
  
  
  #fixing date format for new_data
  new_data$Date <- mdy(new_data$Date)
  
  
  #restricting wilmers_data to 
  #' wolf kills and fresh scavenges
  #' those observed from before stage 2.5
  wilmers_carcmeta <- read_xlsx("data/wilmers_carcass_site_data.xlsx")
  
  wilmers_carcmeta <- merge(wilmers_carcmeta, carcass_meta, by.x = 6, by.y = 1)
  wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS") |
                                         wilmers_carcmeta$COD %in% c("DEFINITE WOLF", "PROBABLE WOLF"),]
  wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$`starting point` <= 2.5,]
  wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$`Mortality Number` != "00-200" &
                                         wilmers_carcmeta$`Mortality Number` != "00-126" &
                                         wilmers_carcmeta$`Mortality Number` != "00-203",]
  
  #merging wilmers_data with metadata to 
  #' add kill#
  #' date of death
  #' prey species
  #' prey age
  wilmers_data <- merge(wilmers_carcmeta[,c(1,2,7,8,25)], wilmers_data, by.x = 2, by.y = 2)
  
  
  #changing species identifier in Wilmer's data
  wilmers_data[wilmers_data$SpeciesID == "R",]$SpeciesID <- "RAVEN"
  wilmers_data[wilmers_data$SpeciesID == "M",]$SpeciesID <- "MAGPIE"
  wilmers_data[wilmers_data$SpeciesID == "C",]$SpeciesID <- "COYOTE"
  wilmers_data[wilmers_data$SpeciesID == "BE",]$SpeciesID <- "BALD EAGLE"
  wilmers_data[wilmers_data$SpeciesID == "GE",]$SpeciesID <- "GOLDEN EAGLE"
  
  
  #fixing column names
  names(new_data)[names(new_data) %in% colnames(new_data[,c(1,3,4,6,7,19:21)])] <- c("Mort", "Date", "Time", "SpeciesID", "NumberofAnimals", "DOD", "Prey", "Age")
  names(wilmers_data)[names(wilmers_data) %in% colnames(wilmers_data[,c(2,7,8,10,11,5,3,4)])] <- c("Mort", "Prey", "Age", "DOD", "Date", "Time", "SpeciesID", "NumberofAnimals")
  
  
  #extracting times from wilmers_data
  #fixing date format for wilmers_data
  wilmers_data$Time <- format(wilmers_data$Time, format = "%H:%M")
  wilmers_data$Date <- as.Date(wilmers_data$Date)
  
  
  #restricting data to winter months
  new_data <- new_data[month(new_data$Date) == 11 | month(new_data$Date) == 12 | 
                         month(new_data$Date) == 1 | month(new_data$Date) == 2 | 
                         month(new_data$Date) == 3,]
  wilmers_data <- wilmers_data[month(wilmers_data$Date) == 11 | month(wilmers_data$Date) == 12 | 
                                 month(wilmers_data$Date) == 1 | month(wilmers_data$Date) == 2 | 
                                 month(wilmers_data$Date) == 3,]
  
  
  #combining wilmers_data and new_data into one dataframe
  data <- rbind(new_data[,c(1,3,4,6,7,19:21)], wilmers_data[,c(2,7,8,10,11,5,3,4)])
  #data <- new_data[,c(1,3,4,6,7,18:20)]
  
  #adding column for 'days from DOD' (deltaDOD)
  #and removing rows with no DOD
  data <- data[!is.na(data$DOD),]
  data$deltaDOD <- as.numeric(data$Date - as.Date(data$DOD))
  
  
  #removing rows with all NA
  data <- data[!is.na(data$Mort),]
  
  
  #removing yearlings (sample size too small) and NA age class from data
  data <- data[data$Age != "YEARLING",]
  data <- data[data$Age != "NA",]
  data <- data[complete.cases(data[,c(1,4,5,7:9)]),]



# [ALTERNATIVE] Reading in and organizing ONLY new_data -------------------
data <- read.csv("All Obs.csv")
data <- data[year(mdy(data$Date)) >= 2021,]


#merging new_data with meta data to add
#' date of death
#' prey species and age
new_carcmeta <- read_xlsx("Initial Site Info.xlsx")
data <- merge(data, new_carcmeta[,c(1,8:10)], by.x = 2, by.y = 1)


#removing cougar kills and late scavenges
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
data <- data[-which(data$Kill.. %in% c(cat_mort, "22-113", "25-1", "21-181")),]


#fixing column names
data <- data[,c(1,3,4,6,7,18:20)]
colnames(data) <- c("Mort", "Date", "Time", "SpeciesID", "NumberofAnimals", "DOD", "Prey", "Age")


#adding column for 'days from DOD' (deltaDOD)
#and removing rows with no DOD
data <- data[!is.na(data$DOD),]
data$deltaDOD <- as.numeric(mdy(data$Date) - as.Date(data$DOD))


#removing rows with all NA
data <- data[!is.na(data$Mort),]


#removing yearlings (sample size too small) and NA age class from data
data <- data[data$Age != "YEARLING",]
data <- data[data$Age != "NA",]
data <- data[complete.cases(data[,c(1,4,5,7:9)]),]



#fixing $Time format
data$Time <- times(paste(data$Time, "00", sep = ":"))


###Restricting to only a single day###
data <- data[data$deltaDOD == 0,]



# Getting combined counts between spatial areas during an observation period -----------------------------

raw_to_model <- function(data, species){
  #subsetting to desired species
  tmp_data <- data[data$SpeciesID == species,]
  
  #creating a variable length list to put unique date/time combinations into
  dattime <- unique(tmp_data[,-c(4,5)])
  dattime_list <- vector("list", length = nrow(dattime))
  
  
  #placing unique date/time rows into their designated list spot
  for(i in 1:nrow(dattime)){
    dattime_list[[i]] <- tmp_data[tmp_data$Date == dattime[i,]$Date & 
                                    tmp_data$Time == dattime[i,]$Time &
                                    tmp_data$Mort == dattime[i,]$Mort,]
  }
  
  
  #calcualting the total count for each time step
  #and creating new dataframe with new combined counts
  dattime$Count <- unlist(lapply(dattime_list, function(x){sum(x$NumberofAnimals)}))
  
  
  #finding the time with the highest count for each carcass
  mort <- unique(dattime$Mort)
  mort_list <- vector("list", length = length(mort))
  
  for(i in 1:length(mort_list)){
    mort_list[[i]] <- dattime[dattime$Mort == mort[i],]
  }
  
  
  #calculating the max count for each time step
  #and creating new dataframe with new combined counts
  mort_df <- do.call(rbind, lapply(mort_list, function(x){x[x$Count == max(x$Count),]}))
  
  
  return(mort_df)
}

#' running function
#' making sure all rows have the necessary data
#' time
#' prey species & age
#' count
raven_data <- raw_to_model(data, "RAVEN")
raven_dattime <- unique(data[data$SpeciesID == "RAVEN",c(1,3,5,6,7)])
magpie_data <- raw_to_model(data, "MAGPIE")
magpie_dattime <- unique(data[data$SpeciesID == "MAGPIE",c(1,3,5,6,7)])
coyote_data <- raw_to_model(data, "COYOTE")
coyote_dattime <- unique(data[data$SpeciesID == "COYOTE",c(1,3,5,6,7)])
baea_data <- raw_to_model(data, "BALD EAGLE")
baea_dattime <- unique(data[data$SpeciesID == "BALD EAGLE",c(1,3,5,6,7)])
goea_data <- raw_to_model(data, "GOLDEN EAGLE")
goea_dattime <- unique(data[data$SpeciesID == "GOLDEN EAGLE",c(1,3,5,6,7)])



# Creating dataframe for models  -------------------------------------------
#performing goodness of fit tests on the number of timesteps (2 hour) with the highest max concurrent count
#including multiple samples from a carcass if they all had the same max count

#' models for elk as prey 
  ##elk-------------
    ###raven----
    raven_elk <- vector(length = 16)
    names(raven_elk) <- as.character(5:20)
    
    p_raven_elk <- vector(length = 16)
    names(p_raven_elk) <- as.character(5:20)
    
    for(i in 1:12){
      even <- 5:24
      raven_elk[i] <- sum(as.numeric(sub("\\:.*","", raven_data[raven_data$Prey == "ELK",]$Time)) == even[i])
      
      p_raven_elk[i] <- sum(as.numeric(sub("\\:.*","", raven_dattime[raven_dattime$Prey == "ELK",]$Time)) == even[i])
    }
    
    
    #removing 0 observation count values from vectors
    p_raven_elk <- p_raven_elk[which(raven_elk != 0)]
    raven_elk <- raven_elk[raven_elk != 0]
    
    
    #goodness of fit chi-squared test
    raven_elk_gof <- chisq.test(raven_elk, correct = F, p = p_raven_elk/sum(p_raven_elk))
    
    
    ###magpie----
    magpie_elk <- vector(length = 16)
    names(magpie_elk) <- as.character(5:20)
    
    p_magpie_elk <- vector(length = 16)
    names(p_magpie_elk) <- as.character(5:20)
    
    for(i in 1:12){
      even <- seq(0, 22, 2)
      magpie_elk[i] <- sum(as.numeric(sub("\\:.*","", magpie_data[magpie_data$Prey == "ELK",]$Time)) == even[i])
      
      p_magpie_elk[i] <- sum(as.numeric(sub("\\:.*","", magpie_dattime[magpie_dattime$Prey == "ELK",]$Time)) == even[i])
    }
    
    
    #removing 0 observation count values from vectors
    p_magpie_elk <- p_magpie_elk[which(magpie_elk != 0)]
    magpie_elk <- magpie_elk[magpie_elk != 0]
    
    
    #goodness of fit chi-squared test
    magpie_elk_gof <- chisq.test(magpie_elk, correct = F, p = p_magpie_elk/sum(p_magpie_elk))
    
    
    ###coyote----
    coyote_elk <- vector(length = 16)
    names(coyote_elk) <- as.character(5:20)
    
    p_coyote_elk <- vector(length = 16)
    names(p_coyote_elk) <- as.character(5:20)
    
    for(i in 1:12){
      even <- seq(0, 22, 2)
      coyote_elk[i] <- sum(as.numeric(sub("\\:.*","", coyote_data[coyote_data$Prey == "ELK",]$Time)) == even[i])
      
      p_coyote_elk[i] <- sum(as.numeric(sub("\\:.*","", coyote_dattime[coyote_dattime$Prey == "ELK",]$Time)) == even[i])
    }
    
    
    #removing 0 observation count values from vectors
    p_coyote_elk <- p_coyote_elk[which(coyote_elk != 0)]
    coyote_elk <- coyote_elk[coyote_elk != 0]
    
    
    #goodness of fit chi-squared test
    coyote_elk_gof <- chisq.test(coyote_elk, correct = F, p = p_coyote_elk/sum(p_coyote_elk))
    
    
    ###bald eagle----
    baea_elk <- vector(length = 16)
    names(baea_elk) <- as.character(5:20)
    
    p_baea_elk <- vector(length = 16)
    names(p_baea_elk) <- as.character(5:20)
    
    for(i in 1:12){
      even <- seq(0, 22, 2)
      baea_elk[i] <- sum(as.numeric(sub("\\:.*","", baea_data[baea_data$Prey == "ELK",]$Time)) == even[i])
      
      p_baea_elk[i] <- sum(as.numeric(sub("\\:.*","", baea_dattime[baea_dattime$Prey == "ELK",]$Time)) == even[i])
    }
    
    
    #removing 0 observation count values from vectors
    p_baea_elk <- p_baea_elk[which(baea_elk != 0)]
    baea_elk <- baea_elk[baea_elk != 0]
    
    
    #goodness of fit chi-squared test
    baea_elk_gof <- chisq.test(baea_elk, correct = F, p = p_baea_elk/sum(p_baea_elk))
    
    
    ###golden eagle----
    goea_elk <- vector(length = 16)
    names(goea_elk) <- as.character(5:20)
    
    p_goea_elk <- vector(length = 16)
    names(p_goea_elk) <- as.character(5:20)
    
    for(i in 1:12){
      even <- seq(0, 22, 2)
      goea_elk[i] <- sum(as.numeric(sub("\\:.*","", goea_data[goea_data$Prey == "ELK",]$Time)) == even[i])
      
      p_goea_elk[i] <- sum(as.numeric(sub("\\:.*","", goea_dattime[goea_dattime$Prey == "ELK",]$Time)) == even[i])
    }
    
    
    #removing 0 observation count values from vectors
    #since it was impacting chi-squared test
    p_goea_elk <- p_goea_elk[which(goea_elk != 0)]
    goea_elk <- goea_elk[goea_elk != 0]
    
    
    #goodness of fit chi-squared test
    goea_elk_gof <- chisq.test(goea_elk, correct = F, p = p_goea_elk/sum(p_goea_elk),
                               simulate.p.value = T, B = 100000)   #B = number of simulated values
    

  ##GoF TEST RESULTS----
    #results from Wilmers+new data
    raven_elk_gof  #p = 0.01666
    magpie_elk_gof #p = 0.8774
    coyote_elk_gof #p = 0.3819
    baea_elk_gof   #p = 0.1447
    goea_elk_gof   #p <0.05 with 100,000 simulated values (0.0104 with 1 mil sims)


    