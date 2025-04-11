#visualizing the daily high count data

library(ggplot2)

#reading in data
source("scripts/exploratory/daily highcount camera.R")
source("scripts/exploratory/daily highcount.R")


#combining obs and camera data
wolf_data <- rbind(wolf_obs_data, wolf_cam_data)
raven_data <- rbind(raven_obs_data, raven_cam_data)
magpie_data <- rbind(magpie_obs_data, magpie_cam_data)
coyote_data <- rbind(coyote_obs_data, coyote_cam_data)
baea_data <- rbind(baea_obs_data, baea_cam_data)
goea_data <- rbind(goea_obs_data, goea_cam_data)


# basic visualization (wolf vs cat kills) ---------------------------------

#there are negative deltadod vaues


raven_data %>% 
  group_by(cougar_kill, delta_dod) %>% 
  summarize(mean = mean_count, range = range(count))
raven_data %>%
  filter(delta_dod <= 10) %>% 
  ggplot() +
  geom_point(aes(x = delta_dod, y = count, col = cougar_kill))

