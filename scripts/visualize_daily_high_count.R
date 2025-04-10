#visualizing the daily high count data

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

