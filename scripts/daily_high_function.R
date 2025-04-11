daily_high_count <- function(data, full_data){
  #only using relevant columns 
  data <- data %>% 
    select("kill_num", "date", "time", "count", 
           "dod", "delta_dod", "species", "sex", 
           "age_class", "cougar_kill")
    
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
  
    obs_period <- full_data %>% 
      group_by(kill_num) %>% 
      dplyr::summarize(min = min(date), max = max(date))
    
    mort_df <- do.call("rbind", lapply(mort_list_max, function(x) {
      obs_dates <- obs_period %>%
        filter(kill_num == as.character(x[1, "kill_num"]))
      obs_dates <- seq(obs_dates$min, obs_dates$max, 'days')
      
      if (length(obs_dates) != nrow(x)) {
        missing_dates <- obs_dates[-which(obs_dates %in% x$date)]
        for (i in 1:length(missing_dates)) {
          x <- x %>%
            bind_rows(
              data.frame(
                kill_num = x[1,]$kill_num,
                date = missing_dates[i],
                time = NA,
                count = 0,
                dod = x[1,]$dod,
                delta_dod = as.numeric(missing_dates[i] - x[1, ]$dod),
                species = x[1,]$species,
                sex = x[1,]$sex,
                age_class = x[1,]$age_class,
                cougar_kill = x[1,]$cougar_kill
              )
            )
        }
      }
      return(x)
    }))
  
    #fixing bug where kills a species is never detected at are registered as 0 counts
    

      full_mort <- unique(full_data$kill_num)
      
      obs_period <- full_data %>% 
        group_by(kill_num) %>% 
        dplyr::summarize(min = min(date), max = max(date))
      
        #which kills are missing for this species
        missing_kills <- full_mort[-which(full_mort %in% mort_df$kill_num)]
        
        if(length(missing_kills) != 0){
          #add the missing days/kills back into the dataset
          for(i in 1:length(missing_kills)){
            kill_meta <- subset(full_data, kill_num == missing_kills[i])[1,]
            
            sample_dates <- subset(obs_period, kill_num == missing_kills[i])
            obs_dates <- seq(sample_dates$min, sample_dates$max, 'days')
            
            mort_df <- mort_df %>% 
              bind_rows(
                data.frame(
                  kill_num = missing_kills[i],
                  date = obs_dates,
                  time = NA,
                  count = 0,
                  dod = kill_meta$dod,
                  delta_dod = as.numeric(obs_dates - kill_meta$dod),
                  species = kill_meta$species,
                  sex = kill_meta$sex,
                  age_class = kill_meta$age_class,
                  cougar_kill = kill_meta$cougar_kill
                )
              )
          }
        }
      
  return(mort_df)
}

#problem where kills don't register as 0 if the species is never detected
