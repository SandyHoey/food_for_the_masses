#creating daily high counts from the cougar kill cameras 
#prep data by creating an artificial smapling interval
#look for a photo taken within +- 2 min of a 10 min interval

library(data.table)

file_names <- list.files("data/cougar_cam_data_files")[grep("csv", list.files("data/cougar_cam_data_files"))]

#need to be careful with 23-302 since there are also ground observations