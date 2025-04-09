library(janitor)



# Reading in data and organizing columns ----------------------------------
data <- clean_names(read.csv("data/new_scavenger_count.csv"))
data$date <- mdy(data$date)
data <- data[year(data$date) >= 2021,]


#merging new_data with meta data to add
#' date of death
#' prey species, sex, age
carcass_meta <- clean_names(read.csv("data/wolf_project_carcass_data.csv"))
data <- merge(data, carcass_meta[,c("kill", "dod", 
                                    "species", "sex", 
                                    "age_class",
                                    "cougar_kill")], 
              by.x = 2, by.y = 1)

#removing late scavenges
#21-181 bison road kill I practiced my counting on
#22-113 F210 kill I started but didn't finish because viewshed was too poor
#21-198 Wapiti scavenge from bison that was partially frozen in BLacktail pond
data <- data[-which(data$kill %in% c("22-113", "21-181", "21-198")),]

#reducing columns and fixing column name
data <- data[,c(1,3:7,19:23)]
colnames(data)[c(1,11)] <- c("kill_num", "cougar_kill")

#removing FOV from count
## ONLY DO IF COMPARING TO CAMERAS
data <- filter(data, area_id != "FIELD OF VIEW")

#adding column for 'days from DOD' (delta_dod)
#and removing rows with no DOD
data <- data[!is.na(data$dod),]
data$dod <- mdy(data$dod)
data$delta_dod <- as.numeric(data$date - data$dod)


### Getting combined counts between spatial areas during a observation period -----------------------------
  
  raw_to_model <- function(data, species){
    #subsetting to desired species
    tmp_data <- data[data$species_id == species,]
    
    #creating a variable length list to put unique date/time combinations into
    dattime <- unique(tmp_data[,-c(4,5)])
    dattime_list <- vector("list", length = nrow(dattime))
    
    
    #placing unique date/time rows into their designated list spot
    for(i in 1:nrow(dattime)){
      dattime_list[[i]] <- tmp_data[tmp_data$date == dattime[i,]$date & 
                                  tmp_data$time == dattime[i,]$time &
                                  tmp_data$kill_num == dattime[i,]$kill_num,]
    }
    
    
    #calculating the total count for each time step
    #and creating new dataframe with new combined counts
    dattime$count <- unlist(lapply(dattime_list, function(x){sum(x$number_of_animals)}))
    
    #finding the high count for each day (delta_dod)
    mort <- unique(dattime$kill_num)
    mort_list <- vector("list", length = length(mort))
    
    for(i in 1:length(mort_list)){
      mort_list[[i]] <- dattime[dattime$kill_num == mort[i],]
    }
    
    #calculating the max count for each time step
    #if multiple observations in a day have the same max count, only taking one of them
    mort_list_max <- lapply(mort_list, function(x) {
      ddd <- unique(x$delta_dod)
      
      ddd_high <- slice(x, 0)
      
      for (i in 1:length(ddd)) {
        ddd_high <- filter(x, delta_dod == ddd[i]) %>%
          filter(count == max(count)) %>%
          slice(1) %>%
          bind_rows(ddd_high)
        
      }
      return(ddd_high)
    })
    
    
    #fixing bug where days observed, but without a count for a species doesn't pull a zero
      #because there is no associated data point
    
    obs_period <- data %>% 
      group_by(kill_num) %>% 
      dplyr::summarize(min = min(date), max = max(date))
    
    mort_df <- do.call("rbind", lapply(mort_list_max, function(x) {
      obs_dates <- obs_period %>% 
        filter(kill_num == x[1,"kill_num"])
      obs_dates <- seq(obs_dates$min, obs_dates$max, 'days')
      
      if(length(obs_dates) != nrow(x)){
        missing <- obs_dates[-which(obs_dates %in% x$date)]
        for(i in 1:length(missing)){
          x <- rbind(x, data.frame(
                          kill_num = x[1,]$kill_num,
                          date = missing[i],
                          time = NA,
                          number_of_animals = 0,
                          dod = x[1,]$dod,
                          species = x[1,]$species,
                          sex = x[1,]$sex,
                          age_class = x[1,]$age_class,
                          cougar_kill = x[1,]$cougar_kill,
                          delta_dod = missing[i] - x[1,]$dod,
                          count = 0))
        }
      }
      return(x)
    }))
    
    return(mort_df)
  }
  
    #' running function
    #' making sure all rows have the necessary data
      #' mort #
      #' prey species & age
      #' delta_dod
      #' count
    wolf_data <- raw_to_model(data, "WOLF")
    wolf_dattime <- unique(data[data$species_id == "WOLF" & data$delta_dod %in% unique(wolf_data$delta_dod), c(1,5,7:9)])
    raven_data <- raw_to_model(data, "RAVEN")
    raven_dattime <- unique(data[data$species_id == "RAVEN" & data$delta_dod %in% unique(raven_data$delta_dod), c(1,5,7:9)])
    magpie_data <- raw_to_model(data, "MAGPIE")
    magpie_dattime <- unique(data[data$species_id == "MAGPIE" & data$delta_dod %in% unique(magpie_data$delta_dod),c(1,5,7:9)])
    coyote_data <- raw_to_model(data, "COYOTE")
    coyote_dattime <- unique(data[data$species_id == "COYOTE" & data$delta_dod %in% unique(coyote_data$delta_dod),c(1,5,7:9)])
    baea_data <- raw_to_model(data, "BALD EAGLE")
    baea_dattime <- unique(data[data$species_id == "BALD EAGLE" & data$delta_dod %in% unique(baea_data$delta_dod),c(1,5,7:9)])
    goea_data <- raw_to_model(data, "GOLDEN EAGLE")
    goea_dattime <- unique(data[data$species_id == "GOLDEN EAGLE" & data$delta_dod %in% unique(goea_data$delta_dod),c(1,5,7:9)])
  
    
###23-051 is messed up and not linking to the correct kill
    #the kill wasn't entered into the database
    #unknown deer
    