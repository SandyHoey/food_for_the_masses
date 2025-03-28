#Comparing air survey scavenger counts to 2021-2022 winter study scavenger counts

library(lubridate)
library(data.table)
library(ggplot2)
library(AER)  #dispersiontest() for glm
library(MASS) #glm.nb

se <- function(x){sqrt(var(x) / length(x))}
se.na <- function(x){sqrt(var(x, na.rm = T) / (length(x)-sum(is.na(x))))}


# Setting up FFTM wolf acquired carcasses ---------------------------------
source("Average Species Count.R")

high_count <- as.data.frame(high_count)
high_count$Kill <- rownames(high_count)
high_count$Study <- "Intensive"


#removing cougar and late scavenged carcasses
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
high_count <- high_count[-which(rownames(high_count) %in% cat_mort),]
high_count <- high_count[rownames(high_count) != "25-1" 
                         & rownames(high_count) != "21-181"
                         & rownames(high_count) != "22-113",]


#creating average species counts
high_count_average <- data.frame(Average = colMeans(high_count[,1:5]),  
                                 se = sapply(high_count[,1:5], se),
                                 study = "FFTM", species = colnames(high_count[,1:5]))


# Setting up data for ground observed carcasses ---------------------------
#reading in carcass detection data (includes scavenger counts)
carcass.all <- read.csv("data/wolf_project_carcass_data.csv")


#fixing date format
carcass.all$DOD <- mdy(carcass.all$DOD)
carcass.all$DATE.DETECTED.GROUND <- mdy(carcass.all$DATE.DETECTED.GROUND)


#subsetting to carcasses observed by ground crew Nov 21 - Mar 22
carcass.winter <- carcass.all[carcass.all$DOD > "2021/11/14" 
                              & carcass.all$DOD < "2022/4/1",]
carcass.winter <- carcass.winter[!is.na(carcass.winter$DOD),]


#subsetting to northern range carcasses
carcass.nr <- carcass.winter[carcass.winter$N.RANGE == "YES",]


#subsetting to ground crew observed carcasses
carcass.ground <- carcass.nr[carcass.nr$PACK.TYPE %like% "GROUND",]



#simplifying dataframe
ground.count <- carcass.ground[,c(1, 38, 39, 35, 40, 41)]
colnames(ground.count) <- c("Kill", "Raven", "Magpie", "Coyote", 
                              "Bald Eagle", "Golden Eagle")
ground.count[2:6] <- sapply(ground.count[2:6],as.numeric)


#removing carcasses that were also watched by FFTM
#the recorded species high counts for ground are often impacted by FFTM since they would ask me what I saw
ground.count <- ground.count[-which(ground.count$Kill %in% high_count$Kill),]


#removing rows with no scavenger counts
#and turning 999 to 0
ground.count[is.na(ground.count)] <- 0
ground.count[ground.count == 999] <- NA
ground.count <- ground.count[rowSums(ground.count[,2:6], na.rm = T) != 0,]
ground.count$Study <- "Opportunistic"


#creating dataframe (long format) of average counts
ground.count.average <- data.frame(Average = colMeans(ground.count[,2:6]), 
                                   se = sapply(ground.count[,2:6], se.na),
                                   study = "WS Ground", species = colnames(ground.count[,2:6]))



# Comparison of max concurrent count --------------------------------------------------------

#creating a dataframe for the model data
model.data <- rbind(ground.count, high_count[, c(8, 1:5, 9)])

  ## t-test ----
  #using independent two sampled t-test, no carcass is in both sample group
  #paired t-test doesn't allow for only partly duplicated sample group
  #using equal variance if var are less than 4:1
  
    #'raven
    t.test(high_count[,1], ground.count[,2], alternative = "two.sided", paired = F, var.equal = F, conf.int = T)
    #' variance not equal (602 vs 75)
      var(high_count[,1])
      var(ground.count[,2], na.rm = T)
    #' normally distributed
      shapiro.test(high_count[,1])
      shapiro.test(ground.count[,2])
    
    
    #'magpie
    wilcox.test(high_count[,2], ground.count[,3], alternative = "two.sided", conf.int = T)
     #' variance not equal (65 vs 16)
      var(high_count[,2])
      var(ground.count[,3], na.rm = T)
    #' not normally distributed (both)
      shapiro.test(high_count[,2])
      shapiro.test(ground.count[,3])
    
    
    #'coyote
    wilcox.test(high_count[,3], ground.count[,4], alternative = "two.sided", paired = F, conf.int = T)
    #' variance not equal (3 vs 2)
      var(high_count[,3])
      var(ground.count[,4], na.rm = T)
    #' not normally distributed (WS)
      shapiro.test(high_count[,3])
      shapiro.test(ground.count[,4])
    
    
    #'bald eagle
    wilcox.test(high_count[,4], ground.count[,5], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (1.7 vs 1)
      var(high_count[,4])
      var(ground.count[,5], na.rm = T)
    #' not normally distributed (WS)
      shapiro.test(high_count[,4])
      shapiro.test(ground.count[,5])
    
    
    #'golden eagle
    wilcox.test(high_count[,5], ground.count[,6], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (2.6 vs .67)
      var(high_count[,5])
      var(ground.count[,6], na.rm = T)
    #' not normally distributed (both)
      shapiro.test(high_count[,5])
      shapiro.test(ground.count[,6])


  ## GLM ----
  #' dispersiontest(_glm)
  #' raven & magpie are overdispersed 
  #' both eagles not overdispersed (overdispersion function p > 0.05)
    #'raven
    raven_glm <- glm.nb(Raven ~ study, data = model.data[,c(1, 2, 7)])
    summary(raven_glm)
    summary(glm.nb(Raven ~ 1, data = model.data[,c(1, 2, 7)]))$aic
    #AIC 244.89 vs 267.6887 (null)
    
    #'magpie
    magpie_glm <- glm.nb(Magpie ~ study, data = model.data[,c(1, 3, 7)])
    summary(magpie_glm)
    summary(glm.nb(Magpie ~ 1, data = model.data[,c(1, 3, 7)]))$aic
    #AIC 184.58 vs 190.8988 (null)
    
    #'coyote
    coyote_glm <- glm(Coyote ~ study, data = model.data[,c(1, 4, 7)], family="poisson")
    summary(coyote_glm)
    summary(glm(Coyote ~ 1, data = model.data[,c(1, 4, 7)]))$aic
    #AIC 123.66 vs 157.183 (null)
    
    #'bald eagle
    baea_glm <- glm(`Bald Eagle` ~ study, data = model.data[,c(1, 5, 7)], family = "poisson")
    summary(baea_glm)
    summary(glm(`Bald Eagle` ~ 1, data = model.data[,c(1, 5, 7)], family = "poisson"))$aic
    #AIC 90.28 vs 107.1625 (null) 
    
    #'golden eagle
    goea_glm <- glm(`Golden Eagle` ~ study, data = model.data[,c(1, 6, 7)], family = "poisson")
    summary(goea_glm)
    summary(glm(`Golden Eagle` ~ 1, data = model.data[,c(1, 6, 7)], family = "poisson"))$aic
    #AIC 84.082 vs 90.87887 (null)
    
  

  
  
# Comparison (MCC) Plots --------------------------------------------------------------
  #' raw data averages + SE
  plot.data.average <- rbind(high_count_average, ground.count.average)
    
  ggplot(data = plot.data.average, aes(x = species, y = Average, fill = study)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Average-se, ymax = Average+se), width=.2,
                  position=position_dodge(.9)) + 
    scale_fill_discrete(name="Survey Method",
                        breaks=c("FFTM", "WS Ground"),
                        labels=c("Intensive Ground", "Opportunistic Ground")) +
    xlab("")
    
  
  #' boxplot
  cb <- c("#66C2A5","#8DA0CB")
  plot.data.all <- rbind(high_count[,c(1:5,9)], ground.count[,2:7])
  boxplot_data <- melt(setDT(plot.data.all), id.vars = 6, measure.vars = 1:5)
  names(boxplot_data)[2:3] <- c("Species", "Count")
    #creating data frame to annotate each facet to show if it was significant or not
    ann_df <- data.frame(Species = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"),
                         label = c("*", "*", "*", "*", ""),
                         y_level = c(103.7, 30, 7.5, 5.2, 5.2))
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
  
  
  
  #Trend line for winter study counts
  #northern range
  #winter months
  #elk and bison only
  #removed carcasses count using intensive method during 2021-2022 winter season
  ggplot(data=carcass.ground, aes(y=as.numeric(..OF.RAVEN), x=year(DOD))) + ylim(0,80) + xlim(1996,2021) +
    geom_point() + geom_smooth(method = lm, level=.99) + 
    labs(y="Maximum Concurrent\nCount of Ravens", x="Year") +
    theme_classic() + theme(text = element_text(size=14))
  
  
  ggplot(data=carcass.ground, aes(y=as.numeric(..OF.BALD.EAGLE), x=year(DOD))) + ylim(0,10) + xlim(1996,2021) +
    geom_point() + geom_smooth(method = lm, level=.99) + 
    labs(y="Maximum Concurrent\nCount of Ravens", x="Year") +
    theme_classic() + theme(text = element_text(size=14))
  
  