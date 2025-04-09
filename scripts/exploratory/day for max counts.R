  #finding which day and time within that day would likely offer the highest 
  #concurrent scavenger count 
  
  library(lubridate)
  library(readxl)
  library(data.table)
  library(plyr)
  
  
  
  # Reading, Compiling, and organizing raw data ---------------------------------
  #reading in data
  new_data <- read.csv("data/new_scavenger_count.csv")
  wilmers_data <- as.data.frame(read_xlsx("data/wilmers_scavenger_count.xlsx"))
  
  
  #restricting new_data to definite, probable wolf kills and fresh scavenges
  carcass_meta <- read.csv("data/wolf_project_carcass_data.csv", header = T)
  new_carcmeta <- read_xlsx("data/new_carcass_site_data.xlsx")
  
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
  #' wolf kills
  wilmers_carcmeta <- read_xlsx("wilmers_carcass_site_data.xlsx")
  
  wilmers_carcmeta <- merge(wilmers_carcmeta, carcass_meta, by.x = 6, by.y = 1)
  wilmers_carcmeta <- wilmers_carcmeta[wilmers_carcmeta$KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS") |
                                         wilmers_carcmeta$COD %in% c("DEFINITE WOLF", "PROBABLE WOLF"),]
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
  data <- read.csv("data/new_scavenger_count.csv")
  data <- data[year(mdy(data$Date)) >= 2021,]
  
  
  #merging new_data with meta data to add
  #' date of death
  #' prey species and age
  carcass_meta <- read.csv("data/wolf_project_carcass_data.csv", header = T)
  data <- merge(data, carcass_meta[,c(1,8:10)], by.x = 2, by.y = 1)
  
  
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
  
  
  
  # Getting combined counts between spatial areas during a observation period -----------------------------
  
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
    
    
    #calculating the total count for each time step
    #and creating new dataframe with new combined counts
    dattime$Count <- unlist(lapply(dattime_list, function(x){sum(x$NumberofAnimals)}))
    
    
    #finding the deltaDOD with the highest count for each carcass
    mort <- unique(dattime$Mort)
    mort_list <- vector("list", length = length(mort))
    
    for(i in 1:length(mort_list)){
      mort_list[[i]] <- dattime[dattime$Mort == mort[i],]
    }
    
    
    #calculating the max count for each time step
    #if multiple observations in a day have the same max count, only taking one of them
    mort_df <- do.call(rbind, lapply(mort_list, 
                                     function(x){if(length(unique(x$deltaDOD)) == 1){
                                       x[x$Count == max(x$Count),][1,]
                                     }else{
                                       tmp_max <- max(x$Count)
                                       tmp_x_df <- data.frame()
                                       
                                       for(x_day in unique(x$deltaDOD)){
                                         tmp_x <- x[x$deltaDOD == x_day,]
                                         if(tmp_max %in% tmp_x$Count){tmp_x_df <- rbind(tmp_x_df, tmp_x[tmp_x$Count == tmp_max,][1,])}
                                       }
                                       return(tmp_x_df)
                                     }
                                     }))
    
    #calculating the max count for each time step
    #and creating new dataframe with new combined counts
    #mort_df <- do.call(rbind, lapply(mort_list, function(x){x[x$Count == max(x$Count),]}))
  
    
    return(mort_df)
  }
  
    #' running function
    #' making sure all rows have the necessary data
    #' mort #
    #' prey species & age
    #' deltaDOD
    #' count
    raven_data <- raw_to_model(data, "RAVEN")
    raven_dattime <- unique(data[data$SpeciesID == "RAVEN" & data$deltaDOD %in% unique(raven_data$deltaDOD), c(1,5,7:9)])
    magpie_data <- raw_to_model(data, "MAGPIE")
    magpie_dattime <- unique(data[data$SpeciesID == "MAGPIE" & data$deltaDOD %in% unique(magpie_data$deltaDOD),c(1,5,7:9)])
    coyote_data <- raw_to_model(data, "COYOTE")
    coyote_dattime <- unique(data[data$SpeciesID == "COYOTE" & data$deltaDOD %in% unique(coyote_data$deltaDOD),c(1,5,7:9)])
    baea_data <- raw_to_model(data, "BALD EAGLE")
    baea_dattime <- unique(data[data$SpeciesID == "BALD EAGLE" & data$deltaDOD %in% unique(baea_data$deltaDOD),c(1,5,7:9)])
    goea_data <- raw_to_model(data, "GOLDEN EAGLE")
    goea_dattime <- unique(data[data$SpeciesID == "GOLDEN EAGLE" & data$deltaDOD %in% unique(goea_data$deltaDOD),c(1,5,7:9)])
  
  
  # Creating dataframe for models  -------------------------------------------
  #performing goodness of fit tests on the number of deltaDOD with the highest max concurrent count
  #probabilities are forced as the proportion of points for each deltaDOD day since each won't have had the
    #same number of observations made for many reasons (stopping counts early, carcass finishing early. etc)
  #multiple days may have the same high count and will both be included
    
    #' models for elk as prey 
      ##ALL elk-------------
        ###raven----
        raven_elk <- table(raven_data[raven_data$Prey == "ELK",]$deltaDOD)
        
        
        #calculating probabilities
        p_raven_elk <- table(raven_dattime[raven_dattime$Prey == "ELK",]$deltaDOD)/
          nrow(raven_dattime[raven_dattime$Prey == "ELK",])
        
        
        #goodness of fit chi-squared test
        chisq.test(raven_elk, correct = F, p = p_raven_elk,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###magpie----
        magpie_elk <- table(magpie_data[magpie_data$Prey == "ELK",]$deltaDOD)
        
        
        #calculating probabilities
        p_magpie_elk <- table(magpie_dattime[magpie_dattime$Prey == "ELK",]$deltaDOD)/
          nrow(magpie_dattime[magpie_dattime$Prey == "ELK",])
        
        
        #goodness of fit chi-squared test
        chisq.test(magpie_elk, correct = F, p = p_magpie_elk,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###coyote----
        coyote_elk <- table(coyote_data[coyote_data$Prey == "ELK",]$deltaDOD)
        
        
        #calculating probabilities
        p_coyote_elk <- table(coyote_dattime[coyote_dattime$Prey == "ELK",]$deltaDOD)/
          nrow(coyote_dattime[coyote_dattime$Prey == "ELK",])
        
        
        #goodness of fit chi-squared test
        chisq.test(coyote_elk, correct = F, p = p_coyote_elk,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###bald eagle----
        baea_elk <- table(baea_data[baea_data$Prey == "ELK",]$deltaDOD)
        
        
        #calculating probabilities
        p_baea_elk <- table(baea_dattime[baea_dattime$Prey == "ELK",]$deltaDOD)/
          nrow(baea_dattime[baea_dattime$Prey == "ELK",])
        
        
        #goodness of fit chi-squared test
        chisq.test(baea_elk, correct = F, p = p_baea_elk,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###golden eagle----
        goea_elk <- table(goea_data[goea_data$Prey == "ELK",]$deltaDOD)
        
        
        #calculating probabilities
        p_goea_elk <- table(goea_dattime[goea_dattime$Prey == "ELK",]$deltaDOD)/
          nrow(goea_dattime[goea_dattime$Prey == "ELK",])
        
        
        #goodness of fit chi-squared test
        chisq.test(goea_elk, correct = F, p = p_goea_elk,
                   simulate.p.value = T, B = 10000)
        
        
        
      ##adult elk-------------
        ###raven----
        raven_elk_adult <- table(raven_data[raven_data$Prey == "ELK" & raven_data$Age == "ADULT",]$deltaDOD)
        
        
        #calculating probabilities
        p_raven_elk_adult <- table(raven_dattime[raven_dattime$Prey == "ELK" & raven_dattime$Age == "ADULT",]$deltaDOD)/
          nrow(raven_dattime[raven_dattime$Prey == "ELK" & raven_dattime$Age == "ADULT",])
        
        
        #goodness of fit chi-squared test
        chisq.test(raven_elk_adult, correct = F, p = p_raven_elk_adult,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###magpie----
        magpie_elk_adult <- table(magpie_data[magpie_data$Prey == "ELK" & magpie_data$Age == "ADULT",]$deltaDOD)
        
        
        #removing 0 observation count values from vectors
        p_magpie_elk_adult <- table(magpie_dattime[magpie_dattime$Prey == "ELK" & magpie_dattime$Age == "ADULT",]$deltaDOD)/
          nrow(magpie_dattime[magpie_dattime$Prey == "ELK" & magpie_dattime$Age == "ADULT",])
        
        
        #goodness of fit chi-squared test
        chisq.test(magpie_elk_adult, correct = F, p = p_magpie_elk_adult,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###coyote----
        coyote_elk_adult <- table(coyote_data[coyote_data$Prey == "ELK" & coyote_data$Age == "ADULT",]$deltaDOD)
        
        
        #calculating probabilities
        p_coyote_elk_adult <- table(coyote_dattime[coyote_dattime$Prey == "ELK" & coyote_dattime$Age == "ADULT",]$deltaDOD)/
          nrow(coyote_dattime[coyote_dattime$Prey == "ELK" & coyote_dattime$Age == "ADULT",])
  
        
        #goodness of fit chi-squared test
        chisq.test(coyote_elk_adult, correct = F, p = p_coyote_elk_adult,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###bald eagle----
        baea_elk_adult <- table(baea_data[baea_data$Prey == "ELK" & baea_data$Age == "ADULT",]$deltaDOD)
        
        
        #calculating probabilities
        p_baea_elk_adult <- table(baea_dattime[baea_dattime$Prey == "ELK" & baea_dattime$Age == "ADULT",]$deltaDOD)/
          nrow(baea_dattime[baea_dattime$Prey == "ELK" & baea_dattime$Age == "ADULT",])
        
        
        #goodness of fit chi-squared test
        chisq.test(baea_elk_adult, correct = F, p = p_baea_elk_adult,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###golden eagle----
        goea_elk_adult <- table(goea_data[goea_data$Prey == "ELK" & goea_data$Age == "ADULT",]$deltaDOD)
        
        
        #calculating probabilities
        p_goea_elk_adult <- table(goea_dattime[goea_dattime$Prey == "ELK" & goea_dattime$Age == "ADULT",]$deltaDOD)/
          nrow(goea_dattime[goea_dattime$Prey == "ELK" & goea_dattime$Age == "ADULT",])
        
        
        #goodness of fit chi-squared test
        chisq.test(goea_elk_adult, correct = F, p = p_goea_elk_adult,
                   simulate.p.value = T, B = 10000)
        
        
        
      ##calf elk-------------
        ###raven----
        raven_elk_calf <- table(raven_data[raven_data$Prey == "ELK" & raven_data$Age == "CALF",]$deltaDOD)
        
        
        #calculating probabilities
        p_raven_elk_calf <- table(raven_dattime[raven_dattime$Prey == "ELK" & raven_dattime$Age == "CALF",]$deltaDOD)/
          nrow(raven_dattime[raven_dattime$Prey == "ELK" & raven_dattime$Age == "CALF",])
        
        
        #goodness of fit chi-squared test
        chisq.test(raven_elk_calf, correct = F, p = p_raven_elk_calf,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###magpie----
        magpie_elk_calf <- table(magpie_data[magpie_data$Prey == "ELK" & magpie_data$Age == "CALF",]$deltaDOD)
        
        
        #calculating probabilities
        p_magpie_elk_calf <- table(magpie_dattime[magpie_dattime$Prey == "ELK" & magpie_dattime$Age == "CALF",]$deltaDOD)/
          nrow(magpie_dattime[magpie_dattime$Prey == "ELK" & magpie_dattime$Age == "CALF",])
        
        
        #goodness of fit chi-squared test
        chisq.test(magpie_elk_calf, correct = F, p = p_magpie_elk_calf,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###coyote----
        coyote_elk_calf <- table(coyote_data[coyote_data$Prey == "ELK" & coyote_data$Age == "CALF",]$deltaDOD)
        
        
        #calculating probabilities
        p_coyote_elk_calf <- table(coyote_dattime[coyote_dattime$Prey == "ELK" & coyote_dattime$Age == "CALF",]$deltaDOD)/
          nrow(coyote_dattime[coyote_dattime$Prey == "ELK" & coyote_dattime$Age == "CALF",])
        
        
        #goodness of fit chi-squared test
        chisq.test(coyote_elk_calf, correct = F, p = p_coyote_elk_calf,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###bald eagle----
        baea_elk_calf <- table(baea_data[baea_data$Prey == "ELK" & baea_data$Age == "CALF",]$deltaDOD)
        
        
        #calculating probabilities
        p_baea_elk_calf <- table(baea_dattime[baea_dattime$Prey == "ELK" & baea_dattime$Age == "CALF",]$deltaDOD)/
          nrow(baea_dattime[baea_dattime$Prey == "ELK" & baea_dattime$Age == "CALF",])
        
        
        #goodness of fit chi-squared test
        chisq.test(baea_elk_calf, correct = F, p = p_baea_elk_calf,
                   simulate.p.value = T, B = 10000)
        
        
        
        ###golden eagle----
        goea_elk_calf <- table(goea_data[goea_data$Prey == "ELK" & goea_data$Age == "CALF",]$deltaDOD)
        
        
        #calculating probabilities
        p_goea_elk_calf <- table(goea_dattime[goea_dattime$Prey == "ELK" & goea_dattime$Age == "CALF",]$deltaDOD)/
          nrow(goea_dattime[goea_dattime$Prey == "ELK" & goea_dattime$Age == "CALF",])
        
        
        #goodness of fit chi-squared test
        chisq.test(goea_elk_calf, correct = F, p = p_goea_elk_calf,
                   simulate.p.value = T, B = 10000)
        
        
        