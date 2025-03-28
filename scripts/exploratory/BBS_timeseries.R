#creating lineplots for CORA/BBMA detected during BBS routes

library(ggplot2)
library(plyr)

#reading in data and spliting by species (CORA, BBMA)
bbs_data <- read.csv("data/BBS/BBS_Count.csv")
cora_data <- bbs_data[bbs_data$AOU == 4860,]
bbma_data <- bbs_data[bbs_data$AOU == 4750,]
count_history <- read.csv("data/BBS/Route History.csv")


#limiting count_history to only relevant counts
count_history <- count_history[count_history$Route %in% bbs_data$Route,]


#Loop to append other count years to SPECIES_data
#checking to see if the year and route match a count that is already present in cbc_data
append_hist <- function(data){
  tmp_data <- data
  for(i in 1:nrow(count_history)){
    tmp_count <- count_history[i,]
    
      if(sum(tmp_data[tmp_data$Route == tmp_count$Route,]$Year %in% tmp_count$Year) == 0){
        tmp_data <- rbind.fill(tmp_data, tmp_count)}
  }

tmp_data[is.na(tmp_data$SpeciesTotal),]$SpeciesTotal <- 0
  
  return(tmp_data)
}

cora_data <- append_hist(cora_data)
bbma_data <- append_hist(bbma_data)



#Selecting routes that were used >5 times
#25th quantile for is 5 (CORA) and 5 (BBMA)
#quantile(table(cora_data$Route))
#quantile(table(bbma_data$Route))
cora_route <- unique(cora_data$Route)[table(cora_data$Route) > 5]
bbma_route <- unique(bbma_data$Route)[table(bbma_data$Route) > 5]


#creating empty lists to place timeseries plots for each route
cora_list <- vector("list", length = length(cora_route))
bbma_list <- vector("list", length = length(bbma_route))


#Loop for CORA timeseries plots
for(i in 1:length(cora_route)){
  tmp_route <- cora_data[cora_data$Route == cora_route[i],]
  
  cora_list[[i]] <- ggplot(data = tmp_route, aes(x = Year, y = SpeciesTotal)) +
                      geom_line() + geom_point() + 
                      theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                      labs(x = "Year", y = "Count", title = paste0("Ravens on BBS Route ", cora_route[i])) +
                      theme(plot.title = element_text(hjust = 0.5))
}


#Loop for BBMA timeseries plots
for(i in 1:length(bbma_route)){
  tmp_route <- bbma_data[cora_data$Route == bbma_route[i],]
  
  bbma_list[[i]] <- ggplot(data = tmp_route, aes(x = Year, y = SpeciesTotal)) +
                      geom_line() + geom_point() + 
                      theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
                      labs(x = "Year", y = "Count", title = paste0("Magpies on BBS Route ", bbma_route[i]))+
                      theme(plot.title = element_text(hjust = 0.5))
}


#function to calculate average count in each year between BBS routes
year_average <- function(data){
  
  #pulling all years counted
  years <- unique(data$Year)
  
  #creating a df for each year
  average_count <- data.frame(Year = years, Average_Count=0, sd=NA)
  
  #loop to average counts
  for(i in 1:length(years)){
    tmp_year <- data[data$Year == years[i],]
    
    average_count[i, "Average_Count"] <- mean(tmp_year$SpeciesTotal)
    average_count[i, "sd"] <- sd(tmp_year$SpeciesTotal)
  }
  
  return(average_count)
  
}


#Using function to create average counts 
cora_average <- year_average(cora_data[cora_data$Route %in% cora_route,])
bbma_average <- year_average(bbma_data[bbma_data$Route %in% bbma_route,])


#Timeseries plot of average CORA count
ggplot(data = cora_average, aes(x = Year, y = Average_Count)) +
  geom_line(aes(y = Average_Count)) + geom_point(aes(y = Average_Count)) + #average count (line and points)
  geom_line(aes(y = ifelse(Average_Count-sd > 0, Average_Count-sd, 0)), lty = 2, lwd = .7) + #lower sd line
  geom_line(aes(y = Average_Count+sd), lty = 2, lwd = .7) + #upper sd line
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  labs(x = "Year", y = "Count") +
  scale_x_continuous(breaks = seq(min(cora_average$Year), max(cora_average$Year), by = 10))+
  geom_vline(xintercept = c(1970, 1995), #lines to show years with significant events
             linetype = c(4, 5),
             size = c(.7, .7),
             col= c("red", "blue")) +
  geom_text(label = "Stopped feeding of bears", x = 1974.9, y = 45.5, vjust = 1, size = 3.5, check_overlap = T) +
  geom_text(label = "Reintroduction of wolves", x = 1999.73, y = 45.5, vjust = 1, size = 3.5, check_overlap = T) 


labs(x = "Year", y = "Count", title = "Average Ravens on MT BBS Routes") +
  theme(plot.title = element_text(hjust = 0.5))


#Timeseries plot of average BBMA count
ggplot(data = bbma_average, aes(x = Year)) +
  geom_line(aes(y = Average_Count)) + geom_point(aes(y = Average_Count)) + #average count (line and points)
  geom_line(aes(y = ifelse(Average_Count-sd > 0, Average_Count-sd, 0)), lty = 2, lwd = .7) + #lower sd line
  geom_line(aes(y = Average_Count+sd), lty = 2, lwd = .7) + #upper sd line
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  labs(x = "Year", y = "Count") +
  scale_x_continuous(breaks = seq(min(bbma_average$Year), max(bbma_average$Year), by = 10)) +
  geom_vline(xintercept = c(1970, 1995), #lines to show years with significant events
             linetype = c(4, 5),
             size = c(.7, .7),
             col= c("red", "blue")) +
  geom_text(label = "Stopped feeding of bears", x = 1974.9, y = 55.65, vjust = 1, size = 3.5, check_overlap = T) +
  geom_text(label = "Reintroduction of wolves", x = 1999.73, y = 55.65, vjust = 1, size = 3.5, check_overlap = T) 

labs(x = "Year", y = "Count", title = "Average Magpies on MT BBS Routes")
  theme(plot.title = element_text(hjust = 0.5))
