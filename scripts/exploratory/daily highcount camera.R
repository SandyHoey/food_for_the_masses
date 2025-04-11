#creating daily high counts from the cougar kill cameras 
#prep data by creating an artificial smapling interval
#look for a photo taken within +- 2 min of a 10 min interval

library(dplyr)
library(janitor)
library(lubridate)

#reading in and combining all camera csv files
cam_data <- list.files("data/cougar_cam_data_files", full.names = TRUE)[grep("csv", list.files("data/cougar_cam_data_files"))] %>% 
  lapply(read.csv) %>% 
  do.call("rbind", .) %>% 
  clean_names

#adding a kill number row
#removing photo info columns
cam_data <- cam_data %>% 
  mutate(kill_num = stringr::str_sub(root_folder, 1, 6)) 

#creating separate date and time columns
cam_data <- cam_data %>% 
  mutate(date_time = ymd_hms(date_time)) %>% 
  mutate(date = as.Date(date_time), time = hms(format(date_time, "%H:%M:%S")))


# cleaning weird stuff ----------------------------------------------------

#fixing date issue on 23-005
#camera 275 (photos) is behind by a year
correct_date <- cam_data %>% 
  filter(kill_num == "23-005", photo_camera_id == 275) %>% 
  select("date") %>% 
  +365

cam_data[cam_data$kill_num == "23-005" & 
           cam_data$photo_camera_id == 275,]$date <- correct_date[,1]



# general data manipulation -----------------------------------------------

#removing unnecessary rows
cam_data <-cam_data %>% 
  select(-c(1:3,6,7))


#subsetting to only daytime photos
cam_data <- cam_data %>% 
  filter(day_time == "TRUE") %>% 
  select(-c("day_time"))

#merging new_data with meta data to add
#' date of death
#' prey species, sex, age
carcass_meta <- clean_names(read.csv("data/wolf_project_carcass_data.csv"))
cam_data <- merge(cam_data, carcass_meta[,c("kill", "dod", 
                                    "species", "sex", 
                                    "age_class",
                                    "cougar_kill")], 
              by.x = 13, by.y = 1)


#transforming cougar_kill column into binary
cam_data <- cam_data %>% 
  mutate(cougar_kill = cougar_kill == "XXX")
              

#removing rows for deployment and retrieval of camera
# cam_data <- cam_data %>% 
#   filter(!(comment %in% c("setup",
#                         "set up",
#                         "deployment",
#                         "deployement",
#                         "retrieval")))
#NOT removing this so that it shows if the camera was still working when retrieved


#adding column for 'days from DOD' (delta_dod)
#and removing rows with no DOD
cam_data <- cam_data[!is.na(cam_data$dod),]
cam_data$dod <- mdy(cam_data$dod)
cam_data$delta_dod <- as.numeric(cam_data$date - cam_data$dod)


#transforming to long format data
cam_data <- cam_data %>% 
  tidyr::pivot_longer(3:10, 
                      names_to = "scav_species", 
                      values_to = "count")


# extracting daily high count ---------------------------------------------

#sourcing function
source("scripts/daily_high_function.R")


wolf_cam_data <- cam_data %>% 
  filter(scav_species == "wolf") %>% 
  daily_high_count(cam_data)
raven_cam_data <- cam_data %>% 
  filter(scav_species == "raven") %>% 
  daily_high_count(cam_data)
magpie_cam_data <- cam_data %>% 
  filter(scav_species == "magpie") %>% 
  daily_high_count(cam_data)
coyote_cam_data <- cam_data %>% 
  filter(scav_species == "coyote") %>% 
  daily_high_count(cam_data)
baea_cam_data <- cam_data %>% 
  filter(scav_species == "bald_eagle") %>% 
  daily_high_count(cam_data)
goea_cam_data <- cam_data %>% 
  filter(scav_species == "golden_eagle") %>% 
  daily_high_count(cam_data)


#need to be careful with 23-302 since there are also ground observations
  #should be fine because there is no overlap in the deltaDOD between obs and cam