library(gazepath)
library(dplyr)
library(tidyverse)
#set working directory to fixation or saccade

ground_truth <- data.frame()
avg_data = data.frame()
count_files = 0 

main_subjects = list.dirs('.', recursive = F); #43 big folders
for (i in 1:length(main_subjects)) {
  ext1 = main_subjects[i]
  main_path = paste(getwd(), substr(ext1, 2, nchar(ext1)), sep = "")
  setwd(main_path)
  event_subjects = list.dirs('.', recursive = F); #FV 
  for (j in 1) { #change this to iteratre through length of event subjects if I also want to do task
    ext2 = substr(event_subjects[j], 2, nchar(event_subjects[j]))
    event_path = paste(getwd(), ext2, sep="")
    setwd(event_path)
    type_subjects = list.dirs('.', recursive = F); #binocular or dominant
    len = c(1,3);
    for (k in len) {
      ext3 = substr(type_subjects[k], 2, nchar(type_subjects[k]))
      type_path = paste(getwd(), ext3, sep="")
      setwd(type_path)
      getwd()
      file_list <- list.files(getwd(), pattern="*.csv") 
      for (i in 1:length(file_list)){
        data <- read.csv(file_list[i], header = FALSE)
        count = length(data[,1])
        disp_avg = mean(data[,6])
        new_data = data.frame(count, disp_avg)
        avg_data = rbind(avg_data, new_data)
      }
      avg2_data = data.frame(paste(ext1, ext2), paste(ext3), mean(avg_data$count), mean(avg_data$disp_avg))
      ground_truth = rbind(ground_truth, avg2_data)
      avg2_data = data.frame()
      avg_data = data.frame()
      count_files = count_files + 1
      reset_path = event_path
      setwd(reset_path)
    }
    reset_path = main_path
    setwd(reset_path)
  }
  reset_path = "/Volumes/EyeTrackUAV2/SACCADES"  #change to SACCADES for saccade ground truth 
  setwd(reset_path)
}

ground_truth = ground_truth%>%
  rename(
    name = paste.ext3.,
    main_folder = paste.ext1..ext2.,
    count = mean.avg_data.count.,
    duration = mean.avg_data.disp_avg.
         )%>%
  arrange(name)

write.csv(ground_truth,"/Users/aartheebaskaran/Desktop/Saccade_groundtruth.csv", row.names = FALSE)
    
      