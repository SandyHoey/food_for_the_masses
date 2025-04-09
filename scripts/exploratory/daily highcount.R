library(janitor)

data <- clean_names(read.csv("data/new_scavenger_count.csv"))
data <- data[year(mdy(data$date)) >= 2021,]


#merging new_data with meta data to add
#' date of death
#' prey species, sex, age
carcass_meta <- clean_names(read.csv("data/wolf_project_carcass_data.csv", header = T))
data <- merge(data, carcass_meta[,c("kill", "dod", 
                                    "species", "sex", 
                                    "age_class",
                                    "cougar_kill")], 
              by.x = 2, by.y = 1)

#removing late scavenges
#21-181 bison road kill I practiced my counting on
#22-113 F210 kill I started but didn't finish because viewshed was too poor
data <- data[-which(data$kill %in% c("22-113", "21-181")),]
