#creating daily high counts from the wolf/cougar kill observations 

library(janitor)
library(lubridate)
library(dplyr)



# Reading in data and organizing columns ----------------------------------
obs_data <- clean_names(read.csv("data/new_scavenger_count.csv"))
obs_data$date <- mdy(obs_data$date)
obs_data <- obs_data[year(obs_data$date) >= 2021,]


#merging new_data with meta data to add
#' date of death
#' prey species, sex, age
carcass_meta <- clean_names(read.csv("data/wolf_project_carcass_data.csv"))
obs_data <- merge(obs_data, carcass_meta[,c("kill", "dod", 
                                    "species", "sex", 
                                    "age_class",
                                    "cougar_kill")], 
              by.x = 2, by.y = 1)


#transforming cougar_kill column into binary
obs_data <- obs_data %>% 
  mutate(cougar_kill = cougar_kill == "XXX")

#removing late scavenges
#21-181 bison road kill I practiced my counting on
#22-113 F210 kill I started but didn't finish because viewshed was too poor
#21-198 Wapiti scavenge from bison that was partially frozen in BLacktail pond
obs_data <- obs_data[-which(obs_data$kill %in% c("22-113", "21-181", "21-198")),]

#reducing columns and fixing column name
obs_data <- obs_data[,c(1,3:7,19:23)]
colnames(obs_data)[c(1,5,11)] <- c("kill_num", "scav_species", "cougar_kill")

#removing FOV from count
## ONLY DO IF COMPARING TO CAMERAS
obs_data <- filter(obs_data, area_id != "FIELD OF VIEW")

#adding column for 'days from DOD' (delta_dod)
#and removing rows with no DOD
obs_data <- obs_data[!is.na(obs_data$dod),]
obs_data$dod <- mdy(obs_data$dod)
obs_data$delta_dod <- as.numeric(obs_data$date - obs_data$dod)


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
    wolf_obs_data <- daily_high_count(obs_combine_area(obs_data, "WOLF"), obs_data)
    raven_obs_data <- daily_high_count(obs_combine_area(obs_data, "RAVEN"), obs_data)
    magpie_obs_data <- daily_high_count(obs_combine_area(obs_data, "MAGPIE"), obs_data)
    coyote_obs_data <- daily_high_count(obs_combine_area(obs_data, "COYOTE"), obs_data)
    baea_obs_data <- daily_high_count(obs_combine_area(obs_data, "BALD EAGLE"), obs_data)
    goea_obs_data <- daily_high_count(obs_combine_area(obs_data, "GOLDEN EAGLE"), obs_data)

    