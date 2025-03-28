#comparing scavenger counts at wolf and cougar carcasses


library(ggplot2)
library(MASS)
library(AER)  #dispersiontest() for glm


se <- function(x){sqrt(var(x) / length(x))}

#creating high_counts for each carcass
source("scripts/exploratory/Average Species Count.R")
source("scripts/exploratory/Average Species Count (cat edit).R")

high_count <- as.data.frame(high_count)
high_count$Kill <- rownames(high_count)
cat_high_count <- as.data.frame(cat_high_count)
cat_high_count$Kill <- rownames(cat_high_count)

#subsetting wolf and cat carcasses
cat_mort <- c("22-015", "22-073", "22-115", "23-002", "23-051")
lion_use <- cat_high_count[row.names(cat_high_count) %like% "cat",]
lion_use$Predator <- "Cougar"
wolf_use <- as.data.frame(high_count[-which(rownames(high_count) %in% c("22-113", cat_mort, "25-1", "21-181")),])
wolf_use$Predator <- "Wolf"
high_count <- rbind(lion_use, wolf_use)


#average uses
average_lion_use <- data.frame(Average = colMeans(lion_use[,1:7]), 
                               se = sapply(lion_use[,1:7], se),
                               Predator = "Cougar", species = colnames(lion_use[,1:7]))
average_wolf_use <- data.frame(Average = colMeans(wolf_use[,1:7]), 
                               se = sapply(wolf_use[,1:7], se),
                               Predator = "Wolf", species = colnames(wolf_use[,1:7]))


# Comparison ---------------------------------------------------------


  ## t-test ----
  #using independent two sampled t-test, no carcass is in both sample group
  #using equal variance if var are less than 4:1
    #'raven
    t.test(wolf_use[,1], lion_use[,1], alternative = "two.sided", paired = F, var.equal = T, conf.int = T)
    #' variance equal (602 vs 1161)
      var(wolf_use[,1])
      var(lion_use[,1])
    #' normally distributed 
      shapiro.test(wolf_use[,1])
      shapiro.test(lion_use[,1])
    #' median values (38, 4)
      #median(wolf_use[,1])
      #median(lion_use[,1])
    
    
    #'magpie
    wilcox.test(wolf_use[,2], lion_use[,2], alternative = "two.sided", paired = F, conf.int = T)
    #' variance equal (65 vs 124)
      var(wolf_use[,2])
      var(lion_use[,2])
    #' not normally distributed (wolf)
      shapiro.test(wolf_use[,2])
      shapiro.test(lion_use[,2])
    #' median values (8.5, 18)
      #median(wolf_use[,2])
      #median(lion_use[,2])
    
    
    
    #'coyote
    wilcox.test(wolf_use[,3], lion_use[,3], alternative = "two.sided", paired = F, conf.int = T)
    #' variance not equal (3 vs 0.3)
      var(wolf_use[,3])
      var(lion_use[,3])
    #' not normally distributed (cat) 
      shapiro.test(wolf_use[,3])
      shapiro.test(lion_use[,3])
    #' median values (3, 0)
      #median(wolf_use[,3])
      #median(lion_use[,3])
    
    
    
    #'bald eagle
    wilcox.test(wolf_use[,4], lion_use[,4], alternative = "two.sided", paired = F, conf.int = T)
    #' variance not equal (1.7 vs 0.3)
      var(wolf_use[,4])
      var(lion_use[,4])
    #' not normally distributed (cat)
      shapiro.test(wolf_use[,4])
      shapiro.test(lion_use[,4])
    #' median values (2.5, 2)
      #median(wolf_use[,4])
      #median(lion_use[,4])
    
    
    
    #'golden eagle
    wilcox.test(wolf_use[,5], lion_use[,5], alternative = "two.sided", paired = F, conf.int = T)
    #' variance not equal (2.6 vs 0.3)
      var(wolf_use[,5])
      var(lion_use[,5])
    #' not normally distributed (both)
      shapiro.test(wolf_use[,5])
      shapiro.test(lion_use[,5])
    #' median values (1, 2)
      #median(wolf_use[,5])
      #median(lion_use[,5])
    
    
    
  ## two-sided random permutation test - randomly reshuffling sample group ----
    
    n.sample <- 1000000
    n1 <- nrow(wolf_use)
    n2 <- nrow(lion_use)
    n <- n1 + n2
    
    #raven
    raven.stat <- mean(wolf_use$Raven) - mean(lion_use$Raven)
    raven.sample <- vector(length=n.sample)
    
    for(i in 1:n.sample){
      trial <- sample(high_count$Raven, n, replace=F)
      g1 <- trial[1:n1]
      g2 <- trial[(n1+1):length(trial)]
      raven.sample[i] <- mean(g1) - mean(g2)
    }
    
    hist(raven.sample)
    sum(abs(raven.sample) >= abs(raven.stat))/n.sample  #p = 0.19
  
    
    #magpie
    magpie.stat <- mean(wolf_use$Magpie) - mean(lion_use$Magpie)
    magpie.sample <- vector(length=n.sample)
    
    for(i in 1:n.sample){
      trial <- sample(high_count$Magpie, n, replace=F)
      g1 <- trial[1:n1]
      g2 <- trial[(n1+1):length(trial)]
      magpie.sample[i] <- mean(g1) - mean(g2)
    }
    
    hist(magpie.sample)
    sum(abs(magpie.sample) >= abs(magpie.stat))/n.sample  #p = 0.0.74
    
    
    #coyote
    coyote.stat <- mean(wolf_use$Coyote) - mean(lion_use$Coyote)
    coyote.sample <- vector(length=n.sample)
    
    for(i in 1:n.sample){
      trial <- sample(high_count$Coyote, n, replace=F)
      g1 <- trial[1:n1]
      g2 <- trial[(n1+1):length(trial)]
      coyote.sample[i] <- mean(g1) - mean(g2)
    }
    
    hist(coyote.sample)
    sum(abs(coyote.sample) >= abs(coyote.stat))/n.sample  #p = 0.034
    
    
    #bald eagle
    baea.stat <- mean(wolf_use$`Bald Eagle`) - mean(lion_use$`Bald Eagle`)
    baea.sample <- vector(length=n.sample)
    
    for(i in 1:n.sample){
      trial <- sample(high_count$`Bald Eagle`, n, replace=F)
      g1 <- trial[1:n1]
      g2 <- trial[(n1+1):length(trial)]
      baea.sample[i] <- mean(g1) - mean(g2)
    }
    
    hist(baea.sample)
    sum(abs(baea.sample) >= abs(baea.stat))/n.sample  #p = 0.25
    
    
    #golden eagle
    goea.stat <- mean(wolf_use$`Golden Eagle`) - mean(lion_use$`Golden Eagle`)
    goea.sample <- vector(length=n.sample)
    
    for(i in 1:n.sample){
      trial <- sample(high_count$`Golden Eagle`, n, replace=F)
      g1 <- trial[1:n1]
      g2 <- trial[(n1+1):length(trial)]
      goea.sample[i] <- mean(g1) - mean(g2)
    }
    
    hist(goea.sample)
    sum(abs(goea.sample) >= abs(goea.stat))/n.sample  #p = 1
    
    
    ## GLM ----
    #' dispersiontest(_glm)
    #' coyote & both eagle models were not overdispersed > 0.14 (NOT CHECKED RECENTLY)
    #' other models using negative binomial
    
      #raven
      raven_glm <- glm.nb(Raven ~ Pred, data = high_count)
      summary(raven_glm)
      glm.nb(Raven ~ 1, data = high_count)$aic
      #174.15 vs 173.6561 (null)
      
      
      #magpie
      magpie_glm <- glm.nb(Magpie ~ Pred, data = high_count)
      summary(magpie_glm)
      glm.nb(Magpie ~ 1, data = high_count)$aic
      #124.8 vs 123.7789 (null)
      
      
      #coyote
      coyote_glm <- glm(Coyote ~ Pred, data = high_count, family="poisson")
      summary(coyote_glm)
      glm(Coyote ~ 1, data = high_count)$aic
      #82.582 vs 85.70943 (null)
      
      
      #bald eagle
      baea_glm <- glm(`Bald Eagle` ~ Pred, data = high_count, family="poisson")
      summary(baea_glm)
      glm(`Bald Eagle` ~ 1, data = high_count, family="poisson")$aic
      #66.895 vs 64.96581 (null)
      
      
      #golden eagle
      goea_glm <- glm(`Golden Eagle` ~ Pred, data = high_count, family="poisson")
      summary(goea_glm)
      glm(`Golden Eagle` ~ 1, data = high_count, family="poisson")$aic
      #67.389 vs 66.81962 (null)
    
# Comparison Plots --------------------------------------------------------------
  #' raw data barplot
  barplot.data <- rbind(average_lion_use[1:5,], average_wolf_use[1:5,])
    
  ggplot(data = barplot.data, aes(x = species, y = Average, fill = Predator)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Average-se, ymax = Average+se), width=.2,
                  position=position_dodge(.9)) +
    xlab("")
  
  
  #' raw data boxplot
  cb <- c("#AF0A0A", "#D6CA03")
  boxplot_data <- melt(setDT(high_count[,c(1:5, 9)]), id.vars = 6, measure.vars = 1:5)
  names(boxplot_data)[2:3] <- c("Species", "Count")
    #creating data frame to annotate each facet to show if it was significant or not
    ann_df <- data.frame(Species = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"),
                         label = c("", "", "*", "", ""),
                         y_level = c(103.7, 30, 8, 5.2, 5.2))
    ann_df$Species <- factor(ann_df$Species, levels = c("Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"))
    
  
  p <- ggplot(data = boxplot_data, aes(x = Predator, y = Count)) + 
    geom_boxplot(fill = c(cb, cb, cb, cb, cb)) +
    geom_segment(data = ann_df, inherit.aes = F, aes(x = "Cougar", xend = "Wolf", y = y_level, yend = y_level)) +
    labs(y = "Maximum Concurrent Count", x = "Primary Predator") + 
    facet_wrap(~Species, nrow = 1, strip.position = "bottom", scale = "free_y") +
    theme_classic() + 
    theme(text = element_text(size=14),              #increasing axis font size
          strip.text = element_text(size = 17))      #increasing facet_wrap heading font size
  
  p + geom_text(ann_df, mapping = aes(x = 1.5, y = y_level+y_level/130, label = label), size = 5)
  