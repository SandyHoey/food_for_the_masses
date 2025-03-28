#Cameron Ho
#YNP Food for the Masses
#creating time series plots to show the number of individuals across the use of the carcass


library(lubridate)
library(dplyr)
library(hms)
library(ggplot2)
library(ggpubr)
library(gridGraphics)


# Function and individual species plots -----------------------------------
#reading in data
data <- read.csv("data/new_scavenger_count.csv", header=T)

species_timeseries <- function(mort){
  
  #subsetting the data for the desired mort #
  mort_data <- data[data$Kill.. == mort,]
  days <- unique(mort_data$Date)
  
  #creating a list to store all the finished combined plots
  final_plot_list <- vector("list", 0)
  
  
  
  ######################### RAVEN
  if("RAVEN" %in% mort_data$Species.ID){
    
    #subsetting raven data
    raven_data <- mort_data[mort_data$Species.ID == "RAVEN",]
    
    #combining counts from different areas
    raven_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(raven_data[raven_data$Date == days[d],]) == 0){
        tmp_raven_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    raven_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(raven_count_list[[d]][1,1])){
        
        raven_plot_list[[d]] <- ggplot(data = data.frame(raven_count_list[d]), aes(x = Time, y = Count, group=1)) +
                                        geom_blank() +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") +
                                        ylim(0,max(do.call("rbind", raven_count_list)$Count)) +
                                        scale_x_discrete(drop=F) +
                                        geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        raven_plot_list[[d]] <- ggplot(data = data.frame(raven_count_list[d]), aes(x = Time, y = Count, group=1)) + 
                                        geom_line(size=.8) +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") + 
                                        ylim(0,max(do.call("rbind", raven_count_list)$Count)) +
                                        scale_x_discrete(drop=FALSE)
      }
    }
    
    
    #plotting all days together
    raven_plots <- ggarrange(plotlist = raven_plot_list, 
                            ncol = length(raven_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(raven_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("Raven use of ", mort)))  
    
    
    #creating dataframe (long) of raven count data to plot all species on same graph
    raven_count_df <- do.call("rbind", raven_count_list)
    raven_count_df$Species <- "Raven"
    
  } else(raven_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                      Count = as.integer(), Species = as.character()))

  
  ######################### MAGPIE
  if("MAGPIE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    magpie_data <- mort_data[mort_data$Species.ID == "MAGPIE",]
    
    #combining counts from different areas
    magpie_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(magpie_data[magpie_data$Date == days[d],]) == 0){
        tmp_magpie_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    magpie_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(magpie_count_list[[d]][1,1])){
        
        magpie_plot_list[[d]] <- ggplot(data = data.frame(magpie_count_list[d]), aes(x = Time, y = Count, group=1)) +
                                          geom_blank() +
                                          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                          labs(x="", y="") +
                                          ylim(0,max(do.call("rbind", magpie_count_list)$Count)) +
                                          scale_x_discrete(drop=F) +
                                          geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        magpie_plot_list[[d]] <- ggplot(data = data.frame(magpie_count_list[d]), aes(x = Time, y = Count, group=1)) + 
                                          geom_line(size=.8) +
                                          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                          labs(x="", y="") + 
                                          ylim(0,max(do.call("rbind", magpie_count_list)$Count)) +
                                          scale_x_discrete(drop=FALSE)
      }
    }
    
    
    #plotting all days together
    magpie_plots <- ggarrange(plotlist = magpie_plot_list, 
                            ncol = length(magpie_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(magpie_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("Magpie use of ", mort)))
    
    
    #creating dataframe (long) of magpie count data to plot all species on same graph
    magpie_count_df <- do.call("rbind", magpie_count_list)
    magpie_count_df$Species <- "Magpie"
    
  }else(magpie_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))

  
  ######################### COYOTE
  if("COYOTE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    coyote_data <- mort_data[mort_data$Species.ID == "COYOTE",]
    
    #combining counts from different areas
    coyote_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(coyote_data[coyote_data$Date == days[d],]) == 0){
        tmp_coyote_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    coyote_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(coyote_count_list[[d]][1,1])){
        
        coyote_plot_list[[d]] <- ggplot(data = data.frame(coyote_count_list[d]), aes(x = Time, y = Count, group=1)) +
          geom_blank() +
          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
          labs(x="", y="") +
          ylim(0,max(do.call("rbind", coyote_count_list)$Count)) +
          scale_x_discrete(drop=F) +
          geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        coyote_plot_list[[d]] <- ggplot(data = data.frame(coyote_count_list[d]), aes(x = Time, y = Count, group=1)) + 
          geom_line(size=.8) +
          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
          labs(x="", y="") + 
          ylim(0,max(do.call("rbind", coyote_count_list)$Count)) +
          scale_x_discrete(drop=FALSE)
      }
    }
    
    
    #plotting all days together
    coyote_plots <- ggarrange(plotlist = coyote_plot_list, 
                              ncol = length(coyote_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(coyote_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("Coyote use of ", mort)))
    
    
    #creating dataframe (long) of coyote count data to plot all species on same graph
    coyote_count_df <- do.call("rbind", coyote_count_list)
    coyote_count_df$Species <- "Coyote"
    
  }else(coyote_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))

  
  ######################### BALD EAGLE
  if("BALD EAGLE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    baea_data <- mort_data[mort_data$Species.ID == "BALD EAGLE",]
    
    #combining counts from different areas
    baea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(baea_data[baea_data$Date == days[d],]) == 0){
        tmp_baea_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    baea_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(baea_count_list[[d]][1,1])){
        
        baea_plot_list[[d]] <- ggplot(data = data.frame(baea_count_list[d]), aes(x = Time, y = Count, group=1)) +
                                        geom_blank() +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") +
                                        ylim(0,max(do.call("rbind", baea_count_list)$Count)) +
                                        scale_x_discrete(drop=F) +
                                        geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        baea_plot_list[[d]] <- ggplot(data = data.frame(baea_count_list[d]), aes(x = Time, y = Count, group=1)) + 
                                        geom_line(size=.8) +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") + 
                                        ylim(0,max(do.call("rbind", baea_count_list)$Count))
      }
    }
    
    
    #plotting all days together
    baea_plots <- ggarrange(plotlist = baea_plot_list, 
                            ncol = length(baea_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(baea_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("BAEA use of ", mort)))
    
    
    #creating dataframe (long) of bald eagle count data to plot all species on same graph
    baea_count_df <- do.call("rbind", baea_count_list)
    baea_count_df$Species <- "Bald Eagle"
    
  }else(baea_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))

  
  ######################### GOLDEN EAGLE
  if("GOLDEN EAGLE" %in% mort_data$Species.ID){
    
    #subsetting raven data
    goea_data <- mort_data[mort_data$Species.ID == "GOLDEN EAGLE",]
    
    #combining counts from different areas
    goea_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(goea_data[goea_data$Date == days[d],]) == 0){
        tmp_goea_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    goea_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(goea_count_list[[d]][1,1])){
        
        goea_plot_list[[d]] <- ggplot(data = data.frame(goea_count_list[d]), aes(x = Time, y = Count, group=1)) +
          geom_blank() +
          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
          labs(x="", y="") +
          ylim(0,max(do.call("rbind", goea_count_list)$Count)) +
          scale_x_discrete(drop=F) +
          geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        goea_plot_list[[d]] <- ggplot(data = data.frame(goea_count_list[d]), aes(x = Time, y = Count, group=1)) + 
          geom_line(size=.8) +
          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
          labs(x="", y="") + 
          ylim(0,max(do.call("rbind", goea_count_list)$Count))
      }
    }
    
    
    #plotting all days together
    goea_plots <- ggarrange(plotlist = goea_plot_list, 
                            ncol = length(goea_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(goea_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("GOEA use of ", mort)))
    
    
    #creating dataframe (long) of golden eagle count data to plot all species on same graph
    goea_count_df <- do.call("rbind", goea_count_list)
    goea_count_df$Species <- "Golden Eagle"
    
  }else(goea_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))

  
  ######################### WOLF
  if("WOLF" %in% mort_data$Species.ID){
    
    #subsetting raven data
    wolf_data <- mort_data[mort_data$Species.ID == "WOLF",]
    
    #combining counts from different areas
    wolf_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(wolf_data[wolf_data$Date == days[d],]) == 0){
        tmp_wolf_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    wolf_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(wolf_count_list[[d]][1,1])){
        
        wolf_plot_list[[d]] <- ggplot(data = data.frame(wolf_count_list[d]), aes(x = Time, y = Count, group=1)) +
                                        geom_blank() +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") +
                                        ylim(0,max(do.call("rbind", wolf_count_list)$Count)) +
                                        scale_x_discrete(drop=FALSE) +
                                        geom_hline(yintercept = 0, size = .8)
        } else{
        #plotting timeline
        wolf_plot_list[[d]] <- ggplot(data = data.frame(wolf_count_list[d]), aes(x = Time, y = Count, group=1)) + 
                                        geom_line(size=.8) +
                                        theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                        labs(x="", y="") + 
                                        ylim(0,max(do.call("rbind", wolf_count_list)$Count))
      }
    }
      
    
    #plotting all days together
    wolf_plots <- ggarrange(plotlist = wolf_plot_list, 
                            ncol = length(wolf_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(wolf_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("Wolf use of ", mort)))
    
    
    
    
    #creating dataframe (long) of wolf count data to plot all species on same graph
    wolf_count_df <- do.call("rbind", wolf_count_list)
    wolf_count_df$Species <- "Wolf"
    
  }else(wolf_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))

  
  ######################### COUGAR
  if("MOUNTAIN LION" %in% mort_data$Species.ID){
    
    #subsetting raven data
    cougar_data <- mort_data[mort_data$Species.ID == "MOUNTAIN LION",]
    
    #combining counts from different areas
    cougar_count_list <- vector("list",length(days))
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(nrow(cougar_data[cougar_data$Date == days[d],]) == 0){
        tmp_cougar_data <- data.frame(Date = days[d], Time = NA, Count = NA)
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
    
    #creating a list to place plots into
    cougar_plot_list <- vector("list",length(days))
    
    #loop that creates a plot for each day
    for(d in 1:length(days)){
      
      #for the case when that species was not present on a day that the carcass was still being used
      if(is.na(cougar_count_list[[d]][1,1])){
        
        cougar_plot_list[[d]] <- ggplot(data = data.frame(cougar_count_list[d]), aes(x = Time, y = Count, group=1)) +
                                          geom_blank() +
                                          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                          labs(x="", y="") +
                                          ylim(0,max(do.call("rbind", cougar_count_list)$Count)) +
                                          scale_x_discrete(drop=FALSE) +
                                          geom_hline(yintercept = 0, size = .8)
      } else{
        #plotting timeline
        cougar_plot_list[[d]] <- ggplot(data = data.frame(cougar_count_list[d]), aes(x = Time, y = Count, group=1)) + 
                                          geom_line(size=.8) +
                                          theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                                          labs(x="", y="") + 
                                          ylim(0,max(do.call("rbind", cougar_count_list)$Count))
      }
    }
    
    
    #plotting all days together
    cougar_plots <- ggarrange(plotlist = cougar_plot_list, 
                            ncol = length(cougar_plot_list))
    final_plot_list[[length(final_plot_list)+1]] <- annotate_figure(cougar_plots, 
                                                                    left = text_grob("Count", rot=90),
                                                                    bottom = text_grob("Time"),
                                                                    top = text_grob(paste0("Cougar use of ", mort)))
    
    
    #creating dataframe (long) of cougar count data to plot all species on same graph
    cougar_count_df <- do.call("rbind", cougar_count_list)
    cougar_count_df$Species <- "Cougar"
    
  }else(cougar_count_df <- data.frame(Date= as.character(), Time = as.character(), 
                                     Count = as.integer(), Species = as.character()))
  
  #combining all species count_df to plot all species detected in same plot
  all_count_df <- rbind(raven_count_df, magpie_count_df, coyote_count_df, baea_count_df, 
                              goea_count_df, wolf_count_df, cougar_count_df)
  
  
return(list(final_plot_list, all_count_df))
}



output <- species_timeseries(mort="22-015")

final_plot_list <- output[1]


# Plotting all species together -------------------------------------------
all_count_df <- as.data.frame(output[2])
species_plot_list <- vector("list", length = length(unique(all_count_df$Date)))
days <- unique(all_count_df$Date)


#removing columns with NA time
all_count_df <- all_count_df[-which(is.na(all_count_df$Time)),]


#creating datetime POSIXct column
all_count_df$datetime <- as.POSIXct(paste(mdy(all_count_df$Date), all_count_df$Time), format="%Y-%m-%d %H:%M")


#loop that creates a plot for each day
for(d in 1:length(days)){
  
  #for the case when that species was not present on a day that the carcass was still being used
  #ordering level in chronological
  tmp_day <- data.frame(all_count_df[all_count_df$Date == days[d],])
  tmp_day$Time <- factor(tmp_day$Time, levels = unique(tmp_day[order(tmp_day$datetime),]$Time))
  
  
  
  #ordering species levels to change order of plot legend
  tmp_day$Species <- factor(tmp_day$Species, 
                                 levels = c("Wolf", "Cougar", "Raven", "Magpie", "Coyote", "Bald Eagle", "Golden Eagle"))

  
  #plotting timeline
    species_plot_list[[d]] <- ggplot(data = tmp_day, aes(x = Time, y = Count, color=Species, group=Species)) + theme_classic() +
                                      geom_line(size=0.75) +
                                      theme(axis.text.x = element_text(angle=90, vjust=0.5, size=7.5),
                                            panel.background = element_rect(fill = "red", alpha=0.1)) +
                                      labs(x=days[d], y="") + 
                                      ylim(0,max(all_count_df$Count)) +
                                      scale_x_discrete(drop=FALSE) + 
                                      scale_colour_discrete(drop = FALSE)
                                      
    }


#### adding colored area to show what times were used for cougar calculations ###
    #### 22-115 ###
    species_plot_list[[1]] <- species_plot_list[[1]] + annotate("rect", xmin = "17:35", xmax = "18:35", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
      
    species_plot_list[[2]] <- species_plot_list[[2]] + annotate("rect", xmin = "6:45", xmax = "15:35", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
    
    #### 22-073 ###
    species_plot_list[[1]] <- species_plot_list[[1]] + annotate("rect", xmin = "10:45", xmax = "15:35", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
    
    species_plot_list[[2]] <- species_plot_list[[2]] + annotate("rect", xmin = "8:38", xmax = "16:18", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
    
    species_plot_list[[3]] <- species_plot_list[[3]] + annotate("rect", xmin = "9:48", xmax = "17:25", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
    
    species_plot_list[[4]] <- species_plot_list[[4]] + annotate("rect", xmin = "10:03", xmax = "13:53", 
                                                                ymin = -Inf, ymax = Inf,
                                                                alpha = 0.1, fill = "red")
    
    #### 22-015 ###
    for(i in 1:6){
      species_plot_list[[i]] <- species_plot_list[[i]] + annotate("rect", xmin = -Inf, xmax = Inf, 
                                                                  ymin = -Inf, ymax = Inf,
                                                                  alpha = 0.1, fill = "red")
    }


#plotting all days together
species_plots <- ggarrange(plotlist = species_plot_list, 
                          nrow = 2, ncol = 3,
                          common.legend = TRUE,
                          legend = "right")
annotate_figure(species_plots, 
                left = text_grob("Count", rot=90),
                top = text_grob("Scavenger use of 22-015",))   #CHANGE CARCASS NUMBER

