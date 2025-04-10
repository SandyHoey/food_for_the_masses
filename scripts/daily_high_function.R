daily_high_count <- function(data){
  ## separating into a list by kill number
  mort <- unique(data$kill_num)
  mort_list <- vector("list", length = length(mort))
  
  for(i in 1:length(mort_list)){
    mort_list[[i]] <- data[data$kill_num == mort[i],]
  }
  
  #calculating the max count for each deltadod
  #if multiple observations in a day have the same max count, only taking one of them
  mort_list_max <- lapply(mort_list, function(x) {
    ddd <- unique(x$delta_dod)
    
    ddd_high <- slice(x, 0)
    
    for (i in 1:length(ddd)) {
      ddd_high <- filter(x, delta_dod == ddd[i]) %>%
        filter(count == max(count)) %>%
        slice(1) %>%
        bind_rows(ddd_high)
      
    }
    return(ddd_high)
  })
  
  
  #fixing bug where days observed, but without a count for a species doesn't pull a zero
  #because there is no associated data point
  
  obs_period <- data %>% 
    group_by(kill_num) %>% 
    dplyr::summarize(min = min(date), max = max(date))
  
  mort_df <- do.call("rbind", lapply(mort_list_max, function(x) {
    obs_dates <- obs_period %>% 
      filter(kill_num == x[1,"kill_num"])
    obs_dates <- seq(obs_dates$min, obs_dates$max, 'days')
    
    if(length(obs_dates) != nrow(x)){
      missing <- obs_dates[-which(obs_dates %in% x$date)]
      for(i in 1:length(missing)){
        x <- rbind(x, data.frame(
          kill_num = x[1,]$kill_num,
          date = missing[i],
          time = NA,
          number_of_animals = 0,
          dod = x[1,]$dod,
          species = x[1,]$species,
          sex = x[1,]$sex,
          age_class = x[1,]$age_class,
          cougar_kill = x[1,]$cougar_kill,
          delta_dod = missing[i] - x[1,]$dod,
          count = 0))
      }
    }
    return(x)
  }))
  
  return(mort_df)
}