library(gazepath)
library(dplyr)

#Need to have this in environment before running algorithm
names = c("height", "width", "resx", "resy")
specs = c(294.46,	527.04,	1920,	1080)
screen = data.frame(names, specs)

#Set directory to folder where all the files are 
ground_truth <- data.frame()
avg_data = data.frame()
count_files = 0 

main_subjects = list.dirs('.', recursive = F); #main folders
for (i in 1:length(main_subjects)) {
  ext1 = main_subjects[i]
  main_path = paste(getwd(), substr(ext1, 2, nchar(ext1)), sep = "")
  setwd(main_path)
  for (j in 1) { #change this to iterate through length of event subjects if I also want to do task (right now only looking at free-viewing)
    full_file_list <- list.files(getwd(), pattern="*.csv") 
    indices = seq(1, length(full_file_list), 2)
    file_list = full_file_list[indices]
    for (l in 1:length(file_list)){
        eye_data = read.csv(file_list[l], header = FALSE)

        eye_data = eye_data%>%
          mutate(trial = 1)%>%
          mutate(distance = 1000)
        
        alg_results_gaze = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                            width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "gazepath",
                            samplerate = 1000, thres_dur = 80)
        s_gaze = summary(alg_results_gaze)

        #Getting fixation metrics:
        fix_filtered = s_gaze%>%
          filter(Value == "f")

        fix_count_set = fix_filtered[1]
        fix_count = length(fix_count_set[,1])
        fix_dur_avg = mean(fix_filtered$Duration, na.rm = T)

        # #Getting saccade metrics:
        sac_filtered = s_gaze%>%
          filter(Value == "s")

        sac_count_set = sac_filtered[1]
        sac_count = length(sac_count_set[,1])
        sac_dur_avg = mean(sac_filtered$Duration, na.rm = T)

        # #adding to averaging data set
        new_data = data.frame(fix_count, fix_dur_avg, sac_count, sac_dur_avg)
        avg_data = rbind(avg_data, new_data)
      count_files = count_files + 1
      print(count_files)
    }
    avg2_data = data.frame(paste(ext1), mean(avg_data$fix_count), mean(avg_data$fix_dur_avg), mean(avg_data$sac_count), mean(avg_data$sac_dur_avg))
    ground_truth = rbind(ground_truth, avg2_data)
    avg2_data = data.frame()
    avg_data = data.frame()
  }
reset_path = "/Volumes/EyeTrackUAV2/RAW_DATA/RAW_DATA_EyeTracker_CoordSystem"  # paste in your directory pathway where the folder is - can find this by doing getwd() of the directory after setting it earlier
setwd(reset_path)
}
  
ground_truth = ground_truth%>%
  rename(
    name = paste.ext1.,
    fix_count = mean.avg_data.fix_count.,
    fix_duration = mean.avg_data.fix_dur_avg.,
    sac_count = mean.avg_data.sac_count.,
    sac_duration = mean.avg_data.sac_dur_avg.
  )%>%
  arrange(name)

write.csv(ground_truth,"/Users/aartheebaskaran/Desktop/algorithm-34-43.csv", row.names = FALSE) #change this to your where you want to output the csv with the same title

