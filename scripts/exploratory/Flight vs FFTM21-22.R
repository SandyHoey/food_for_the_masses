#Comparing air survey scavenger counts to 2021-2022 winter study scavenger counts

library(lubridate)
library(data.table)
library(ggplot2)
library(lme4)
library(boot)


se <- function(x){sqrt(var(x, na.rm = T) / length(x))}
se.na <- function(x){sqrt(var(x, na.rm = T) / (length(x)-sum(is.na(x))))}

# Setting up FFTM wolf acquired carcasses ---------------------------------
#creating table for high counts from FFTM21-22
#reading in data
#excluding all counts outside FOV
data <- read.csv("All Obs.csv", header=T)
data <- data[year(mdy(data$Date)) >= 2021,]
data <- data[data$Area.ID != "FIELD OF VIEW" ,]
mort <- unique(data$Kill..)

#creating a list to store all the finished combined plots
high_count <- matrix(nrow = length(mort), ncol = 7, dimnames = list(c(mort), c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle", "Wolf", "Cougar")))

for(i in 1:length(mort)){
  #subsetting the data for the desired mort #
  mort_data <- data[data$Kill.. == mort[i],]
  days <- unique(mort_data$Date)
  
  
  
  
  ######################### RAVEN
  if("RAVEN" %in% mort_data$Species.ID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$Species.ID == "RAVEN",]
    
    #combining counts from different areas
    raven_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(raven_data[raven_data$Date == days[d],]) == 0){
        tmp_raven_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_raven_data$Time <- factor(tmp_raven_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_raven_data <- data.frame(Date = days[d],
                                     Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                     Count = NA)
      }
      
      tmp_times <- tmp_raven_data$Time
      for(t in 1:length(tmp_times)){
        tmp_raven_data[t,3] <- sum(raven_data[raven_data$Date == days[d] & raven_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      raven_count_list[[d]] <- tmp_raven_data
    }
    raven_count_list <- do.call(rbind, raven_count_list)
    
    high_count[i,1] <- max(raven_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("MAGPIE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    magpie_data <- mort_data[mort_data$Species.ID == "MAGPIE",]
    
    #combining counts from different areas
    magpie_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(magpie_data[magpie_data$Date == days[d],]) == 0){
        tmp_magpie_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_magpie_data$Time <- factor(tmp_magpie_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_magpie_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_magpie_data$Time
      for(t in 1:length(tmp_times)){
        tmp_magpie_data[t,3] <- sum(magpie_data[magpie_data$Date == days[d] & magpie_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      magpie_count_list[[d]] <- tmp_magpie_data
    }
    magpie_count_list <- do.call(rbind, magpie_count_list)
    
    high_count[i,2] <- max(magpie_count_list$Count)
  }
  
  
  
  ######################### MAGPIE
  if("COYOTE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    coyote_data <- mort_data[mort_data$Species.ID == "COYOTE",]
    
    #combining counts from different areas
    coyote_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(coyote_data[coyote_data$Date == days[d],]) == 0){
        tmp_coyote_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_coyote_data$Time <- factor(tmp_coyote_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_coyote_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_coyote_data$Time
      for(t in 1:length(tmp_times)){
        tmp_coyote_data[t,3] <- sum(coyote_data[coyote_data$Date == days[d] & coyote_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      coyote_count_list[[d]] <- tmp_coyote_data
    }
    coyote_count_list <- do.call(rbind, coyote_count_list)
    
    high_count[i,3] <- max(coyote_count_list$Count)
  }
  
  
  
  ######################### BALD EAGLE
  if("BALD EAGLE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    baea_data <- mort_data[mort_data$Species.ID == "BALD EAGLE",]
    
    #combining counts from different areas
    baea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(baea_data[baea_data$Date == days[d],]) == 0){
        tmp_baea_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_baea_data$Time <- factor(tmp_baea_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_baea_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_baea_data$Time
      for(t in 1:length(tmp_times)){
        tmp_baea_data[t,3] <- sum(baea_data[baea_data$Date == days[d] & baea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      baea_count_list[[d]] <- tmp_baea_data
    }
    
    baea_count_list <- do.call(rbind, baea_count_list)
    
    high_count[i,4] <- max(baea_count_list$Count)
  }
  
  
  
  ######################### GOLDEN EAGLE
  if("GOLDEN EAGLE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    goea_data <- mort_data[mort_data$Species.ID == "GOLDEN EAGLE",]
    
    #combining counts from different areas
    goea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(goea_data[goea_data$Date == days[d],]) == 0){
        tmp_goea_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_goea_data$Time <- factor(tmp_goea_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_goea_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_goea_data$Time
      for(t in 1:length(tmp_times)){
        tmp_goea_data[t,3] <- sum(goea_data[goea_data$Date == days[d] & goea_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      goea_count_list[[d]] <- tmp_goea_data
    }
    
    goea_count_list <- do.call(rbind, goea_count_list)
    
    high_count[i,5] <- max(goea_count_list$Count)
  }
  
  
  
  ######################### WOLF
  if("WOLF" %in% mort_data$Species.ID){
    
    #subsetting raven data
    wolf_data <- mort_data[mort_data$Species.ID == "WOLF",]
    
    #combining counts from different areas
    wolf_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(wolf_data[wolf_data$Date == days[d],]) == 0){
        tmp_wolf_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_wolf_data$Time <- factor(tmp_wolf_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_wolf_data <- data.frame(Date = days[d],
                                    Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                    Count = NA)
      }
      
      tmp_times <- tmp_wolf_data$Time
      for(t in 1:length(tmp_times)){
        tmp_wolf_data[t,3] <- sum(wolf_data[wolf_data$Date == days[d] & wolf_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      wolf_count_list[[d]] <- tmp_wolf_data
    }
    
    wolf_count_list <- do.call(rbind, wolf_count_list)
    
    high_count[i,6] <- max(wolf_count_list$Count)
  }
  
  
  
  ######################### COUGAR
  if("MOUNTAIN LION" %in% mort_data$Species.ID){
    
    #subsetting raven data
    cougar_data <- mort_data[mort_data$Species.ID == "MOUNTAIN LION",]
    
    #combining counts from different areas
    cougar_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(cougar_data[cougar_data$Date == days[d],]) == 0){
        tmp_cougar_data <- data.frame(Date = NA, Time = NA, Count = NA)
        tmp_cougar_data$Time <- factor(tmp_cougar_data$Time, levels = unique(mort_data[mort_data$Date == days[d],]$Time))
      } else{
        tmp_cougar_data <- data.frame(Date = days[d],
                                      Time = unique(mort_data[mort_data$Date == days[d],]$Time),
                                      Count = NA)
      }
      
      tmp_times <- tmp_cougar_data$Time
      for(t in 1:length(tmp_times)){
        tmp_cougar_data[t,3] <- sum(cougar_data[cougar_data$Date == days[d] & cougar_data$Time == tmp_times[t],]$Number.of.Animals)
      }
      cougar_count_list[[d]] <- tmp_cougar_data
    }
    
    cougar_count_list <- do.call(rbind, cougar_count_list)
    
    high_count[i,7] <- max(cougar_count_list$Count)
  }
}

high_count[is.na(high_count)] <- 0
high_count <- round(high_count, 1)
high_count <- as.data.frame(high_count)
high_count$Kill <- rownames(high_count)
high_count$Study <- "Ground"


#removing cougar and late scavenged carcasses
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
high_count <- high_count[-which(rownames(high_count) %in% cat_mort),]
high_count <- high_count[rownames(high_count) != "25-1" 
                          & rownames(high_count) != "21-181"
                           & rownames(high_count) != "22-113",]


#creating average species count
high_count_average <- data.frame(Average = colMeans(high_count[,1:5]), 
                                 se = sapply(high_count[,1:5], se),
                                 study = "Ground", species = colnames(high_count[,1:5]))


# (no photo) Setting up data for flight observed carcasses ---------------------------
#reading in carcass detection data (includes scavenger counts)
carcass.all <- read.csv("Carcass Data.csv", stringsAsFactors = F)


#fixing date format
carcass.all$DOD <- mdy(carcass.all$DOD)
carcass.all$DATE.DETECTED.AIR <- mdy(carcass.all$DATE.DETECTED.AIR)
carcass.all$DATE.DETECTED.GROUND <- mdy(carcass.all$DATE.DETECTED.GROUND)


#subsetting to Nov 21 - Mar 22
carcass.winter <- carcass.all[carcass.all$DOD > "2021/11/14" 
                                & carcass.all$DOD < "2022/4/1",]
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
flight.count <- carcass.flight[,c(1, 38, 39, 35, 40, 41)]
colnames(flight.count) <- c("Kill", "Raven", "Magpie", "Coyote", 
                            "Bald Eagle", "Golden Eagle")
flight.count[2:6] <- sapply(flight.count[2:6],as.numeric)


#removing rows with no scavenger counts
#and turning 999 to NA
flight.count[is.na(flight.count)] <- 0
flight.count[flight.count == 999] <- NA
flight.count <- flight.count[rowSums(flight.count[,2:6], na.rm = T) != 0 | flight.count$Kill == "22-071",]
flight.count$Study <- "Aerial"


#creating dataframe (long format) of average counts
flight.count.average <- data.frame(Average = colMeans(flight.count[,2:6]), 
                                   se = sapply(flight.count[,2:6], se.na),
                                   study = "WS Flight", species = colnames(flight.count[,2:6]))



# (include photos) Setting up data for flight observed carcasses ---------------------------
#### * in count course indicates assumption of 0 count
#reading in carcass detection data (includes scavenger counts)
carcass.all <- read.csv("data/flight_scavenger_count_with_photo.csv")


#fixing date format
carcass.all$DOD <- mdy(carcass.all$DOD)


#subsetting to northern range carcasses
carcass.nr <- carcass.all[carcass.all$N.RANGE == "YES",]


#simplifying dataframe
flight.count <- carcass.nr[,c(1, 38, 39, 35, 40, 41)]
colnames(flight.count) <- c("Kill", "Raven", "Magpie", "Coyote", 
                            "Bald Eagle", "Golden Eagle")
flight.count[2:6] <- sapply(flight.count[2:6], as.numeric)


#removing rows with no scavenger counts
#and turning 999 to NA
flight.count[is.na(flight.count)] <- 0
flight.count[flight.count == 999] <- NA
flight.count <- flight.count[rowSums(flight.count[,2:6], na.rm = T) != 0 | flight.count$Kill == "22-071",]
flight.count$Study <- "Aerial"


#creating dataframe (long format) of average counts
flight.count.average <- data.frame(Average = colMeans(flight.count[,2:6], na.rm = T), 
                                   se = sapply(flight.count[,2:6], se.na),
                                   study = "WS Flight", species = colnames(flight.count[,2:6]))



# Comparison of max concurrent count -------------------------------------------------------

#creating a dataframe for the model data
model.data <- rbind(flight.count, high_count[, c(8, 1:5, 9)])


  ## t-test (full) ----
  #using independent two sampled t-test
  #some carcasses are in both sample groups, but t-test has no way to handle partial duplication
  #paired t-test doesn't allow for only partly duplicated sample group
  #using equal variance if var are less than 4:1   
  
  #'raven
  wilcox.test(high_count[,1], flight.count[,2], alternative = "two.sided", paired = F, conf.int = T)
  #' variance equal (326 vs 144)
  var(high_count[,1])
  var(flight.count[,2], na.rm = T)
  #' not normally distributed (WS)
  shapiro.test(high_count[,1])
  shapiro.test(flight.count[,2])
  
  
  #'magpie
  wilcox.test(high_count[,2], flight.count[,3], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (39 vs 8)
  var(high_count[,2])
  var(flight.count[,3], na.rm = T)
  #' not normally distributed (WS)
  shapiro.test(high_count[,2])
  shapiro.test(flight.count[,3])
  
  
  #'coyote
  wilcox.test(high_count[,3], flight.count[,4], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (2 vs 0.2)
  var(high_count[,3])
  var(flight.count[,4], na.rm = T)
  #' not normally distributed (WS)
  shapiro.test(high_count[,3])
  shapiro.test(flight.count[,4])
  
  
  #'bald eagle
  wilcox.test(high_count[,4], flight.count[,5], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (1.8 vs 0.2)
  var(high_count[,4])
  var(flight.count[,5], na.rm = T)
  #' not normally distributed (both)
  shapiro.test(high_count[,4])
  shapiro.test(flight.count[,5])
  
  
  #'golden eagle
  wilcox.test(high_count[,5], flight.count[,6], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (1.3 vs 0.3)
  var(high_count[,5])
  var(flight.count[,6], na.rm = T)
  #' not normally distributed (both)
  shapiro.test(high_count[,5])
  shapiro.test(flight.count[,6])
  
  
  ## t-test (short) ----
  #using independent two sampled t-test
  #some carcasses are in both sample groups, but t-test has no way to handle partial duplication
  #paired t-test doesn't allow for only partly duplicated sample group
  #using equal variance if var are less than 4:1   
  
  #'raven
  wilcox.test(high_count[,1], flight.count[,2], alternative = "two.sided", paired = F, conf.int = T)
  #' variance equal (326 vs 130)
    #var(high_count[,1])
    #var(flight.count[,2], na.rm = T)
  #' not normally distributed (WS)
    #shapiro.test(high_count[,1])
    #shapiro.test(flight.count[,2])
  
  
  #'magpie
  wilcox.test(high_count[,2], flight.count[,3], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (38 vs 5)
    #var(high_count[,2])
    #var(flight.count[,3], na.rm = T)
  #' not normally distributed (WS)
    #shapiro.test(high_count[,2])
    #shapiro.test(flight.count[,3])
  
  
  #'coyote
  wilcox.test(high_count[,3], flight.count[,4], alternative = "two.sided", paired = F, conf.int = T)
  #' variance equal (2 vs 0.6)
    #var(high_count[,3])
    #var(flight.count[,4], na.rm = T)
  #' not normally distributed (WS Flight)
    #shapiro.test(high_count[,3])
    #shapiro.test(flight.count[,4])
  
  
  #'bald eagle
  wilcox.test(high_count[,4], flight.count[,5], alternative = "two.sided", paired = F, conf.int = T)
  #' variance not equal (2 vs 0.1)
    #var(high_count[,4])
    #var(flight.count[,5], na.rm = T)
  #' not normally distributed (FFTM, WS Flight)
    #shapiro.test(high_count[,4])
    #shapiro.test(flight.count[,5])
  
  
  #'golden eagle
  wilcox.test(high_count[,5], flight.count[,6], alternative = "two.sided", paired = F, conf.int = T)
  #' variance equal (1.3 vs 0.5)
    #var(high_count[,5])
    #var(flight.count[,6], na.rm = T)
  #' not normally distributed (FFTM, WS Flight)
    #shapiro.test(high_count[,5])
    #shapiro.test(flight.count[,6])
  

  ## GLMM ----
  #using GLMM to account for counts being done from the same carcass (mort #)
  #none are overdispersed (overdispersion function p > 0.19)
    #'raven
    #'had to change optimizer since it was not converging with default
    raven_glmm <- glmer(Raven ~ study + (1|Kill), data = model.data[,c(1, 2, 7)], family = "poisson",
                        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
    summary(raven_glmm)
    summary(glmer(Raven ~ 1 + (1|Kill), data = model.data[,c(1, 2, 7)], family = "poisson",
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))$AIC
    #AIC 336.4 vs 396.494 (null)
    
    #'magpie
    magpie_glmm <- glmer(Magpie ~ study + (1|Kill), data = model.data[,c(1, 3, 7)], family = "poisson")
    summary(magpie_glmm)
    summary(glmer(Magpie ~ 1 + (1|Kill), data = model.data[,c(1, 3, 7)], family = "poisson"))$AIC
    #AIC 222.1 vs 245.6876 (null)
    
    #'coyote
    coyote_glmm <- glmer(Coyote ~ study + (1|Kill), data = model.data[,c(1, 4, 7)], family = "poisson")
    summary(coyote_glmm)
    summary(glmer(Coyote ~ 1 + (1|Kill), data = model.data[,c(1, 4, 7)], family = "poisson"))$AIC
    #AIC 117.7 vs 151.60043 (null)
    
    #'bald eagle
    baea_glmm <- glmer(`Bald Eagle` ~ study + (1|Kill), data = model.data[,c(1, 5, 7)], family = "poisson")
    summary(baea_glmm)
    summary(glmer(`Bald Eagle` ~ 1 + (1|Kill), data = model.data[,c(1, 5, 7)], family = "poisson"))$AIC
    #AIC 69.9 vs 93.2213
    
    #'golden eagle
    goea_glmm <- glmer(`Golden Eagle` ~ study + (1|Kill), data = model.data[,c(1, 6, 7)], family = "poisson")
    summary(goea_glmm)
    summary(glmer(`Golden Eagle` ~ 1 + (1|Kill), data = model.data[,c(1, 6, 7)], family = "poisson"))$AIC
    #AIC 73.7 vs 77.65412 (null)

  
  #using chi squared goodness of fit
    #'raven
    chisq.test(data.frame(WS = mean(model.data[model.data$study == "Aerial",]$Raven),
                          FFTM = mean(model.data[model.data$study == "Ground",]$Raven)), correct = F)
    
    #'magpie
    chisq.test(data.frame(WS = mean(model.data[model.data$study == "Aerial",]$Magpie),
                          FFTM = mean(model.data[model.data$study == "Ground",]$Magpie)), correct = F)
    
    #'coyote
    chisq.test(data.frame(WS = mean(model.data[model.data$study == "Aerial",]$Coyote),
                          FFTM = mean(model.data[model.data$study == "Ground",]$Coyote)), correct = F)
    
    #'bald eagle
    chisq.test(data.frame(WS = mean(model.data[model.data$study == "Aerial",]$`Bald Eagle`),
                          FFTM = mean(model.data[model.data$study == "Ground",]$`Bald Eagle`)), correct = F)
    
    #'golden eagle
    chisq.test(data.frame(WS = mean(model.data[model.data$study == "Aerial",]$`Golden Eagle`),
                          FFTM = mean(model.data[model.data$study == "Ground",]$`Golden Eagle`)), correct = F)
  

  
    
  
# Comparison plots --------------------------------------------------------------

  #' raw data average + SE
  plot.data <- rbind(high_count_average, flight.count.average)
    
  ggplot(data = plot.data, aes(x = species, y = Average, fill = study)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Average-se, ymax = Average+se), width=.2,
                  position=position_dodge(.9)) + 
    scale_fill_discrete(name="Survey Method",
                        breaks=c("FFTM", "WS Flight"),
                        labels=c("Intensive Ground", "Single Time Aerial")) +
    xlab("")

  
  #' boxplot
  cb <- c("#66C2A5","#F0A0C5")
  plot.data.all <- rbind(high_count[,c(1:5,9)], flight.count[,2:7])
  boxplot_data <- melt(setDT(plot.data.all), id.vars = 6, measure.vars = 1:5)
  names(boxplot_data)[2:3] <- c("Species", "Count")
  boxplot_data$Study <- factor(boxplot_data$Study, levels = c("Ground", "Aerial"))
    #creating data frame to annotate each facet to show if it was significant or not
    ann_df <- data.frame(Species = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"),
                         label = c("*", "*", "*", "*", "*"),
                         y_level = c(103.7, 30, 8, 5.2, 5.2))
    ann_df$Species <- factor(ann_df$Species, levels = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"))
  
    
  p <- ggplot(data = boxplot_data, aes(x = Study, y = Count)) + 
    geom_boxplot(fill = c(cb, cb, cb, cb, cb)) +
    geom_segment(data = ann_df, inherit.aes = F, aes(x = 1, xend = 2, y = y_level, yend = y_level)) +
    labs(y = "Maximum Concurrent Count", x = "Survey Method") +
    facet_wrap(~Species, nrow = 1, strip.position = "bottom", scale = "free_y") +
    theme_classic() + 
    theme(text = element_text(size=14),              #increasing axis font size
          strip.text = element_text(size = 17))      #increasing facet_wrap heading font size
  
  
  p + geom_text(ann_df, mapping = aes(x = 1.5, y = y_level+y_level/130, label = label), size = 5)
