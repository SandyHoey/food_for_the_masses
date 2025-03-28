library(tidyverse)

#reading in carcass data
carcass.all <- read.csv("data/wolf_project_carcass_data.csv")


#fixing date format
carcass.all$DOD <- mdy(carcass.all$DOD)
carcass.all$DATE.DETECTED.AIR <- mdy(carcass.all$DATE.DETECTED.AIR)
carcass.all$DATE.DETECTED.GROUND <- mdy(carcass.all$DATE.DETECTED.GROUND)


#subsetting to carcasses observed by flight crew in winter
carcass.all$year <- year(carcass.all$DOD)
carcass.all$month <- month(carcass.all$DOD)
carcass.winter <- carcass.all[carcass.all$month %in% c(11, 12, 1, 2, 3),]
carcass.winter <- carcass.winter[!is.na(carcass.winter$DOD),]


#subsetting to northern range carcasses
carcass.nr <- carcass.winter[carcass.winter$N.RANGE == "YES",]


#subsetting data to carcasses only detected by flights
carcass.flight <- carcass.nr[carcass.nr$PACK.TYPE == "AIR" 
                             | carcass.nr$PACK.TYPE == "AIR & GPS CLUSTER"
                             | carcass.nr$PACK.TYPE == "UNCOLLARED - AIR",]


#Adding scavenger counts from flights when ground observation was also done
add.flight <- read.csv("Flight Scavenger Counts (short).csv", stringsAsFactors = F)
add.flight$DOD <- mdy(add.flight$DOD)
add.flight$DATE.DETECTED.AIR <- mdy(add.flight$DATE.DETECTED.AIR)
add.flight$DATE.DETECTED.GROUND <- mdy(add.flight$DATE.DETECTED.GROUND)
carcass.flight <- rbind(carcass.flight, add.flight)


#simplifying dataframe
flight.count <- carcass.flight[,c(1, 38, 39, 35, 40, 41, 53)]
colnames(flight.count) <- c("Kill", "Raven", "Magpie", "Coyote", 
                            "Bald Eagle", "Golden Eagle", "Year")
flight.count[2:6] <- sapply(flight.count[2:6],as.numeric)


#removing rows with no scavenger counts
#and turning 999 to NA
flight.count[is.na(flight.count)] <- 0
flight.count[flight.count == 999] <- NA
flight.count <- flight.count[rowSums(flight.count[,2:6], na.rm = T) != 0 | flight.count$Kill == "22-071",]
flight.count$Study <- "Aerial"



# Plot --------------------------------------------------------------------

ggplot(data = flight.count, aes(x = Year, y = Raven)) +
  geom_point() + geom_smooth()

