#creating table that shows the percentage of sites that each scavenger species was detected at

library(lubridate)
library(dplyr)

#reading in data
data <- read.csv("data/new_scavenger_count.csv", header=T)
data <- data[year(mdy(data$Date)) >= 2021,]


#removing carcasses that were late scavenges by wolves
#data for all kills except late scavenges
  #data <- data[data$Kill.. != "25-1" & data$Kill.. != "21-181",]
#data for wolf kills only
  data <- data[-which(data$Kill.. %in% c("22-113","22-115", "22-015", "22-073", "25-1", "21-181")),]
#data for cat kills only
  #data <- filter(data, Kill.. %in% c("22-113"," 22-115", "22-015", "22-073"))


#scavenger species
scav <- c("RAVEN", "MAGPIE", "COYOTE", "BALD EAGLE", "GOLDEN EAGLE", "ROUGHLEGGED HAWK", 
          "BOBCAT", "CROW")


#creating a list with a dataframe containing the data for each carcass
scav_list <- vector("list", length = length(scav))

for(i in 1:length(scav)){
  scav_list[[i]] <- data[data$Species.ID == scav[i],]
}

names(scav_list) <- scav


#calculating how many sites had each scavenger
presence <- data.frame(Species = scav, 
                       Count = sapply(scav_list,function(x){length(unique(x$Kill..))}))

presence$Percent <- round(presence$Count/length(unique(data$Kill..))*100,2)

