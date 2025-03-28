#making a plot of historical opportunistic ground counts to see if it follows the same trend as intensive ground


library(lubridate)
library(data.table)
library(ggplot2)



# Setting up data for ground observed carcasses ---------------------------
#reading in carcass detection data (includes scavenger counts)
carcass.all <- read.csv("data/new_scavenger_count.csv")


#fixing date format
carcass.all$DOD <- mdy(carcass.all$DOD)
carcass.all$DATE.DETECTED.GROUND <- mdy(carcass.all$DATE.DETECTED.GROUND)


#subsetting to carcasses observed by ground crew in winter
carcass.all$year <- year(carcass.all$DOD)
carcass.all$month <- month(carcass.all$DOD)
carcass.winter <- carcass.all[carcass.all$month %in% c(11, 12, 1, 2, 3),]
carcass.winter <- carcass.winter[!is.na(carcass.winter$DOD),]


#subsetting to northern range carcasses
carcass.nr <- carcass.winter[carcass.winter$N.RANGE == "YES",]


#subsetting to ground crew observed carcasses
carcass.ground <- carcass.nr[carcass.nr$PACK.TYPE %like% "GROUND",]



#simplifying dataframe
ground.count <- carcass.ground[,c(1, 38, 39, 35, 40, 41, 53)]
colnames(ground.count) <- c("Kill", "Raven", "Magpie", "Coyote", 
                            "Bald Eagle", "Golden Eagle", "Year")
ground.count[2:6] <- sapply(ground.count[2:6],as.numeric)


#removing carcasses that were also watched by FFTM
#the recorded species high counts for ground are often impacted by FFTM since they would ask me what I saw
source("Average Species Count.R")

high_count <- as.data.frame(high_count)
high_count$Kill <- rownames(high_count)
high_count$Study <- "Intensive"


#removing cougar and late scavenged carcasses
high_count <- high_count[high_count[,7] == 0,]
high_count <- high_count[rownames(high_count) != "25-1" 
                         & rownames(high_count) != "21-181",]


#creating average species counts
high_count_average <- data.frame(Average = colMeans(high_count[,1:5]),  
                                 se = sapply(high_count[,1:5], se),
                                 study = "FFTM", species = colnames(high_count[,1:5]))


ground.count <- ground.count[-which(ground.count$Kill %in% high_count$Kill),]


#removing rows with no scavenger counts
#and turning 999 to 0
ground.count[is.na(ground.count)] <- 0
ground.count[ground.count == 999] <- NA
ground.count <- ground.count[rowSums(ground.count[,2:6], na.rm = T) != 0,]
ground.count$Study <- "Opportunistic"



# Plot --------------------------------------------------------------------

ggplot(data = ground.count, aes(x = Year, y = Raven)) +
  geom_point() +geom_smooth()

