#taking the kill data for cats and looking at presence absence of various scavenger species
#comparing to wolves also using the wolf project data set
#precursor analysis to the more fine scale observation/camera data

library(data.table)
library(tidyverse)

data <- read.csv("data/wolf_project_carcass_data.csv")


# cat scavenger presence percentage ---------------------------------------
#pulling cat associated kills out of the greater KILL database
cat_data <- subset(data, Cougar.Kill.. == "XXX" & 
                     N.RANGE == "YES" &
                     COD != "PROBABLE WOLF")
table(cat_data$COD)
#there are 2 likely cougar scavenges. Should probably get rid of them
#!!!but have not done so yet


#subsetting to only the winter months (Nov-Mar)
cat_data$DOD <- mdy(cat_data$DOD)
cat_wint_data <- subset(cat_data, month(cat_data$DOD) %in% c(11, 12, 1, 2, 3))



#Questions on how I should handle the scavenger counts
#should I assume that a scavenger count was attempted for each row?
#should I assume that there were no scavenger counts done from aerial because it is so inconsistent
#should I assume a scavenger count was not conducted for all of the rows that have 'scavenger present' = unknown
#!!!Have not cut out any observations based on this yet


#Creating presence/absence column
cat_wint_data$X..OF.COYOTE <- as.numeric(cat_wint_data$X..OF.COYOTE)
cat_wint_data$coyote.presence <- as.numeric(cat_wint_data$X..OF.COYOTE > 0)
cat_wint_data$coyote.presence[is.na(cat_wint_data$coyote.presence)] <- 0

cat_wint_data$X..OF.RAVEN <- as.numeric(cat_wint_data$X..OF.RAVEN)
cat_wint_data$raven.presence <- as.numeric(cat_wint_data$X..OF.RAVEN > 0)
cat_wint_data$raven.presence[is.na(cat_wint_data$raven.presence)] <- 0

cat_wint_data$X..OF.MAGPIE <- as.numeric(cat_wint_data$X..OF.MAGPIE)
cat_wint_data$magpie.presence <- as.numeric(cat_wint_data$X..OF.MAGPIE > 0)
cat_wint_data$magpie.presence[is.na(cat_wint_data$magpie.presence)] <- 0

cat_wint_data$X..OF.BALD.EAGLE <- as.numeric(cat_wint_data$X..OF.BALD.EAGLE)
cat_wint_data$bald.presence <- as.numeric(cat_wint_data$X..OF.BALD.EAGLE > 0)
cat_wint_data$bald.presence[is.na(cat_wint_data$bald.presence)] <- 0

cat_wint_data$X..OF.GOLDEN.EAGLE <- as.numeric(cat_wint_data$X..OF.GOLDEN.EAGLE)
cat_wint_data$golden.presence <- as.numeric(cat_wint_data$X..OF.GOLDEN.EAGLE > 0)
cat_wint_data$golden.presence[is.na(cat_wint_data$golden.presence)] <- 0

cat_wint_data$X..OF.FOX <- as.numeric(cat_wint_data$X..OF.FOX)
cat_wint_data$fox.presence <- as.numeric(cat_wint_data$X..OF.FOX > 0)
cat_wint_data$fox.presence[is.na(cat_wint_data$fox.presence)] <- 0

#combining  "wolves" and "wolves scavenge" since I dont care about if there were multiple packs or not
cat_wint_data$X..OF.WOLVES <- as.numeric(cat_wint_data$X..OF.WOLVES)
cat_wint_data$X..OF.WOLVES.SCAVENGED <- as.numeric(cat_wint_data$X..OF.WOLVES.SCAVENGED)
cat_wint_data$wolf.presence <- as.numeric(cat_wint_data$X..OF.WOLVES > 0)
cat_wint_data$wolf.presence[which(cat_wint_data$X..OF.WOLVES.SCAVENGED > 0)] <- 1
cat_wint_data$wolf.presence[is.na(cat_wint_data$wolf.presence)] <- 0

#restructuring data frame into long format for scavenger detection
cat_data_long <- melt(setDT(cat_wint_data), id.vars=c(1:52), variable.name="species")


#checking which detection methods were used
table(cat_wint_data$KILL.DISCOVERED)

#removing aerial only counts since they don't attempt scavenger counts
cat_data_long <- cat_data_long[-which(cat_data_long$KILL.DISCOVERED == "AERIAL"),]


#percent of presence at cougar kill by species
tapply(cat_data_long$value, INDEX = cat_data_long$species, function(x){100*(sum(x)/length(x))})


#examining how removing ground observation impacts the presence percentage 
#(different detection rate)
cat_wint_data_cluster <- cat_wint_data[-which(KILL.DISCOVERED %like% "GROUND"),]
cat_data_long_cluster <- melt(setDT(cat_wint_data_cluster), id.vars=c(1:51), variable.name="species")
tapply(cat_data_long_cluster$value, INDEX = cat_data_long_cluster$species, function(x){100*(sum(x)/length(x))})

#this ground data includes ground only detections and the ground+cluster detections
#im not sure which detection method detected the scavengers
cat_wint_data_ground <- subset(cat_wint_data, KILL.DISCOVERED %like% "GROUND")
cat_data_long_ground <- melt(setDT(cat_wint_data_ground), id.vars=c(1:51), variable.name="species")
tapply(cat_data_long_ground$value, INDEX = cat_data_long_ground$species, function(x){100*(sum(x)/length(x))})

# wolf scavenger presence percentage --------------------------------------
#pulling wolf associated kills out of the greater KILL database
wolf_data <- subset(data, KILL.TYPE %in% c("WOLF KILL", "SCAVENGE FRESH CARCASS",
                                           "DEFINITE WOLF", "PROBABLE WOLF") & 
                     N.RANGE == "YES")
table(wolf_data$COD)


#subsetting to only the winter months (Nov-Mar)
wolf_data$DOD <- mdy(wolf_data$DOD)
wolf_wint_data <- subset(wolf_data, month(wolf_data$DOD) %in% c(11, 12, 1, 2, 3))


#Questions on how I should handle the scavenger counts
#should I assume that a scavenger count was attempted for each row?
#should I assume a scavenger count was not conducted for all of the rows that have 'scavenger present' = unknown
#!!!Have not cut out any observations based on this yet


#Creating presence/absence column
wolf_wint_data$X..OF.COYOTE <- as.numeric(wolf_wint_data$X..OF.COYOTE)
wolf_wint_data$coyote.presence <- as.numeric(wolf_wint_data$X..OF.COYOTE > 0)
wolf_wint_data$coyote.presence[is.na(wolf_wint_data$coyote.presence)] <- 0

wolf_wint_data$X..OF.RAVEN <- as.numeric(wolf_wint_data$X..OF.RAVEN)
wolf_wint_data$raven.presence <- as.numeric(wolf_wint_data$X..OF.RAVEN > 0)
wolf_wint_data$raven.presence[is.na(wolf_wint_data$raven.presence)] <- 0

wolf_wint_data$X..OF.MAGPIE <- as.numeric(wolf_wint_data$X..OF.MAGPIE)
wolf_wint_data$magpie.presence <- as.numeric(wolf_wint_data$X..OF.MAGPIE > 0)
wolf_wint_data$magpie.presence[is.na(wolf_wint_data$magpie.presence)] <- 0

wolf_wint_data$X..OF.BALD.EAGLE <- as.numeric(wolf_wint_data$X..OF.BALD.EAGLE)
wolf_wint_data$bald.presence <- as.numeric(wolf_wint_data$X..OF.BALD.EAGLE > 0)
wolf_wint_data$bald.presence[is.na(wolf_wint_data$bald.presence)] <- 0

wolf_wint_data$X..OF.GOLDEN.EAGLE <- as.numeric(wolf_wint_data$X..OF.GOLDEN.EAGLE)
wolf_wint_data$golden.presence <- as.numeric(wolf_wint_data$X..OF.GOLDEN.EAGLE > 0)
wolf_wint_data$golden.presence[is.na(wolf_wint_data$golden.presence)] <- 0

wolf_wint_data$X..OF.FOX <- as.numeric(wolf_wint_data$X..OF.FOX)
wolf_wint_data$fox.presence <- as.numeric(wolf_wint_data$X..OF.FOX > 0)
wolf_wint_data$fox.presence[is.na(wolf_wint_data$fox.presence)] <- 0

#only using wolf scavenger to get the times when a second pack fed
wolf_wint_data$X..OF.WOLVES.SCAVENGED <- as.numeric(wolf_wint_data$X..OF.WOLVES.SCAVENGED)
wolf_wint_data$wolf.presence <- as.numeric(wolf_wint_data$X..OF.WOLVES.SCAVENGED > 0)
wolf_wint_data$wolf.presence[is.na(wolf_wint_data$wolf.presence)] <- 0

#restructuring data frame into long format for scavenger detection
wolf_data_long <- melt(setDT(wolf_wint_data), id.vars=c(1:52), variable.name="species")

#percent of presence at wolf kill by species
tapply(wolf_data_long$value, INDEX = wolf_data_long$species, function(x){100*(sum(x)/length(x))})

#checking to see what detection methods are used
table(wolf_wint_data$KILL.DISCOVERED)


#only ground observations
wolf_wint_data_cluster <- subset(wolf_wint_data, KILL.DISCOVERED %like% "GROUND")
wolf_data_long_cluster <- melt(setDT(wolf_wint_data_cluster), id.vars=c(1:51), variable.name="species")
tapply(wolf_data_long_cluster$value, INDEX = wolf_data_long_cluster$species, function(x){100*(sum(x)/length(x))})


#only cluster
#to match what is used above for the cat data
wolf_wint_data_ground <- subset(wolf_wint_data, KILL.DISCOVERED == "GPS CLUSTER")
wolf_data_long_ground <- melt(setDT(wolf_wint_data_ground), id.vars=c(1:51), variable.name="species")
tapply(wolf_data_long_ground$value, INDEX = wolf_data_long_ground$species, function(x){100*(sum(x)/length(x))})

#may want to combine all eagle and unknown eagle stuff together for the cluster detection
#just to check because you cant tell the difference with just sign


# model -------------------------------------------------------------------


