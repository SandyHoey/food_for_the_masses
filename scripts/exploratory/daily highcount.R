#creating daily high counts from the wolf/cougar kill observations 

library(janitor)
library(lubridate)
library(dplyr)



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
colnames(data)[c(1,5,11)] <- c("kill_num", "scav_species", "cougar_kill")

#removing FOV from count
## ONLY DO IF COMPARING TO CAMERAS
data <- filter(data, area_id != "FIELD OF VIEW")

#adding column for 'days from DOD' (delta_dod)
#and removing rows with no DOD
data <- data[!is.na(data$dod),]
data$dod <- mdy(data$dod)
data$delta_dod <- as.numeric(data$date - data$dod)


### reading functions in -----------------------------
#combine count areas
source("scripts/combine_area_function.R")

#extract day high count
source("scripts/daily_high_function.R")
    

# transforming data -----------------------------------------------------------

    #' running function
    #' making sure all rows have the necessary data
      #' mort #
      #' prey species & age
      #' delta_dod
      #' count
    wolf_data <- daily_high_count(obs_combine_area(data, "WOLF"))
    wolf_dattime <- unique(data[data$scav_species == "WOLF" & data$delta_dod %in% unique(wolf_data$delta_dod), c(1,5,7:9)])
    raven_data <- daily_high_count(obs_combine_area(data, "RAVEN"))
    raven_dattime <- unique(data[data$scav_species == "RAVEN" & data$delta_dod %in% unique(raven_data$delta_dod), c(1,5,7:9)])
    magpie_data <- daily_high_count(obs_combine_area(data, "MAGPIE"))
    magpie_dattime <- unique(data[data$scav_species == "MAGPIE" & data$delta_dod %in% unique(magpie_data$delta_dod),c(1,5,7:9)])
    coyote_data <- daily_high_count(obs_combine_area(data, "COYOTE"))
    coyote_dattime <- unique(data[data$scav_species == "COYOTE" & data$delta_dod %in% unique(coyote_data$delta_dod),c(1,5,7:9)])
    baea_data <- daily_high_count(obs_combine_area(data, "BALD EAGLE"))
    baea_dattime <- unique(data[data$scav_species == "BALD EAGLE" & data$delta_dod %in% unique(baea_data$delta_dod),c(1,5,7:9)])
    goea_data <- daily_high_count(obs_combine_area(data, "GOLDEN EAGLE"))
    goea_dattime <- unique(data[data$scav_species == "GOLDEN EAGLE" & data$delta_dod %in% unique(goea_data$delta_dod),c(1,5,7:9)])
  
    