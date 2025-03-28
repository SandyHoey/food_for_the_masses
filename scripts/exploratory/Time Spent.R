#Cameron Ho
#YNP Food for the Masses
#creating table that shows the amount of time spent observing each carcass

library(lubridate)
library(data.table)


#reading in data
data <- read.csv("data/new_scavenger_count.csv")

#pulling out just my data collected
data <- data[year(mdy(data$Date)) >= 2021,]
data$Date <- mdy(data$Date)
mort <- unique(data$Kill..)
mort <- mort[-which(mort %like% "25-")]


#creating table to store time spent monitoring each carcass
time_monitored <- matrix(nrow=length(mort), ncol=2, dimnames=list(mort,c("Time (minutes)", "Time (hours)")))


#looping to calculate time spent monitoring each carcass
for(i in 1:length(mort)){
  time <- 0
  tmp_data <- data[data$Kill.. == mort[i],]
  tmp_data <- tmp_data[!is.na(tmp_data$Observation.ID),]
  days <- unique(tmp_data$Date)
  
  #looping to calculate time spent monitoring each carcass on each day
  for(j in 1:length(days)){
    tmp_day <- tmp_data[tmp_data$Date == days[j],]
    tmp_time <- unique(tmp_day$Time)
    time <- time + (length(tmp_time)-1)*10
  }
  time_monitored[i,1] <- time
  time_monitored[i,2] <- time_monitored[i,1]/60
}

colSums(time_monitored[-which(rownames(time_monitored) %in% c("21-181", "22-113")),]) #combined time observed
nrow(time_monitored) #number of carcasses
