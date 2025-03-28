#creating lineplots for CORA/BBMA detected during CBC routes

library(readxl)
library(ggplot2)
library(plyr)

#reading in data
cbc_data <- as.data.frame(read_xlsx("data/CBC/MT-CBC_Circle_Species_Report_SQL_updated.xlsx"))
baea_data <- read.csv("CBC/MT-CBC_BAEA")
count_history <- as.data.frame(read_xlsx("data/CBC/1-121-CBC_Count_History_Report.xlsx"))


#limiting count_history to only relevant counts
count_history <- count_history[count_history$Abbrev %in% cbc_data$Abbrev,]


#turning count_yr from the standardized form to actual years
cbc_data$Year <- cbc_data$Count_yr + 1899
baea_data$Year <- baea_data$Count_yr + 1899
count_history$Year <- count_history$Count_yr + 1899


#splitting by species (CORA, BBMA)
cora_data <- cbc_data[cbc_data$COM_NAME == "Common Raven",]
bbma_data <- cbc_data[cbc_data$COM_NAME == "Black-billed Magpie",]


#Loop to append other count years to SPECIES_data
#checking to see if the year and abbrev match a count that is already present in cbc_data
append_hist <- function(data){
  tmp_data <- data
  for(i in 1:nrow(count_history)){
    tmp_count <- count_history[i,]
    
    if(sum(tmp_data[tmp_data$Abbrev == tmp_count$Abbrev,]$Year %in% tmp_count$Year) == 0){
      tmp_data <- rbind.fill(tmp_data, tmp_count)}
  }
  
  tmp_data[is.na(tmp_data$how_many),]$how_many <- 0
  
  return(tmp_data)
}

cora_data <- append_hist(cora_data)
bbma_data <- append_hist(bbma_data)
baea_data <- append_history(baea_data)


#Selecting counts that were done >8 times
#25th quantile for is 8 (CORA) and 8 (BBMA)
quantile(table(cora_data$Name))
cora_count <- unique(cora_data$Name)[table(cora_data$Name) > 8]

quantile(table(bbma_data$Name))
bbma_count <- unique(bbma_data$Name)[table(bbma_data$Name) > 8]

quantile(table(baea_data$Name))
baea_count <- unique(baea_data$Name)[table(baea_data$Name) > }


#creating empty lists to place timeseries plots for each route
cora_list <- vector("list", length = length(cora_count))
bbma_list <- vector("list", length = length(bbma_count))
baea_list <- vector("list", length = length(baea_count))


#Loop for CORA timeseries plots
for(i in 1:length(cora_count)){
  tmp_count <- cora_data[cora_data$Name == cora_count[i],]
  
  cora_list[[i]] <- ggplot(data = tmp_count, aes(x = Year, y = how_many)) +
    geom_line() + geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x = "Year", y = "Count", title = paste0("Ravens on CBC ", cora_count[i])) +
    theme(plot.title = element_text(hjust = 0.5))
}



#Loop for BBMA timeseries plots
for(i in 1:length(bbma_count)){
  tmp_count <- bbma_data[bbma_data$Name == bbma_count[i],]
  
  bbma_list[[i]] <- ggplot(data = tmp_count, aes(x = Year, y = how_many)) +
    geom_line() + geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x = "Year", y = "Count", title = paste0("Magpies on CBC ", bbma_count[i]))+
    theme(plot.title = element_text(hjust = 0.5))
}




#Loop for BAEA timeseries plots
for(i in 1:length(baea_count)){
  tmp_count <- baea_data[baea_data$Name == baea_count[i],]
  
  baea_list[[i]] <- ggplot(data = tmp_count, aes(x = Year, y = how_many)) +
    geom_line() + geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    labs(x = "Year", y = "Count", title = paste0("Bald Eagle on CBC ", baea_count[i]))+
    theme(plot.title = element_text(hjust = 0.5))
}

#function to calculate average count in each year between CBC routes
year_average <- function(data){
  
  #pulling all years counted
  years <- unique(data$Year)
  
  #creating a df for each year
  average_count <- data.frame(Year = years, Average_Count=0, sd=NA)
  
  #loop to average counts
  for(i in 1:length(years)){
    tmp_year <- data[data$Year == years[i],]
    
    average_count[i, "Average_Count"] <- mean(tmp_year$how_many)
    average_count[i, "sd"] <- sd(tmp_year$how_many)
  }
  
  return(average_count)
  
}


#Using function to create average counts 
cora_average <- year_average(cora_data[cora_data$Name %in% cora_count,])
bbma_average <- year_average(bbma_data[bbma_data$Name %in% bbma_count,])


#Timeseries plot of average CORA count ----
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
  geom_text(label = "Stopped feeding of bears", x = 1959.4, y = 270, vjust = 1, size = 3.5, check_overlap = T) +
  geom_text(label = "Reintroduction of wolves", x = 2005.35, y = 270, vjust = 1, size = 3.5, check_overlap = T) 


labs(x = "Year", y = "Count", title = "Average Ravens on MT CBC") +
  theme(plot.title = element_text(hjust = 0.5))


#Timeseries plot of average BBMA count ----
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
  geom_text(label = "Stopped feeding of bears", x = 1959.4, y = 682, vjust = 1, size = 3.5, check_overlap = T) +
  geom_text(label = "Reintroduction of wolves", x = 2005.35, y = 682, vjust = 1, size = 3.5, check_overlap = T) 
  
labs(x = "Year", y = "Count", title = "Average Magpies on MT CBC") +
  theme(plot.title = element_text(hjust = 0.5))


#Timeseries plot of average BAEA count ----
ggplot(data = baea_average, aes(x = Year)) +
  geom_line(aes(y = Average_Count)) + geom_point(aes(y = Average_Count)) + #average count (line and points)
  geom_line(aes(y = ifelse(Average_Count-sd > 0, Average_Count-sd, 0)), lty = 2, lwd = .7) + #lower sd line
  geom_line(aes(y = Average_Count+sd), lty = 2, lwd = .7) + #upper sd line
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  labs(x = "Year", y = "Count") +
  scale_x_continuous(breaks = seq(min(baea_average$Year), max(baea_average$Year), by = 10)) +
  geom_vline(xintercept = c(1970, 1995), #lines to show years with significant events
             linetype = c(4, 5),
             size = c(.7, .7),
             col= c("red", "blue")) +
  geom_text(label = "Stopped feeding of bears", x = 1959.4, y = 682, vjust = 1, size = 3.5, check_overlap = T) +
  geom_text(label = "Reintroduction of wolves", x = 2005.35, y = 682, vjust = 1, size = 3.5, check_overlap = T) 

labs(x = "Year", y = "Count", title = "Average Bald Eagle on MT CBC") +
  theme(plot.title = element_text(hjust = 0.5))