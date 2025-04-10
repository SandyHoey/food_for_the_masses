obs_combine_area <- function(data, species){
  #subsetting to desired species
  tmp_data <- data[data$scav_species == species,]
  
  #creating a variable length list to put unique date/time combinations into
  dattime <- unique(tmp_data[,-c(4,5)])
  dattime_list <- vector("list", length = nrow(dattime))
  
  
  #placing unique date/time rows into their designated list spot
  for(i in 1:nrow(dattime)){
    dattime_list[[i]] <- tmp_data[tmp_data$date == dattime[i,]$date & 
                                    tmp_data$time == dattime[i,]$time &
                                    tmp_data$kill_num == dattime[i,]$kill_num,]
  }
  
  
  #calculating the total count for each datetime
  #and creating new data frame with new combined area counts
  dattime$count <- unlist(lapply(dattime_list, function(x){sum(x$number_of_animals)}))
  
  return(dattime)
}