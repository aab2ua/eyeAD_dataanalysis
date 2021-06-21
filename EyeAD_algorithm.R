library(gazepath)
library(dplyr)

#Need to have this in environment before running algorithm
names = c("height", "width", "resx", "resy")
specs = c(294.46,	527.04,	1920,	1080)
screen = data.frame(names, specs)

#Set directory to folder where all the files are 
ground_truth <- data.frame()
SD_event_data = data.frame()
RMSE_values = c()
RMSE_data = data.frame()
count_files = 0 

#Creating the RMSE function
RMSE = function(m,o){
  sum = sum(m[o])
  sqrt(sum/30)
}

main_data = list.dirs('.', recursive = F); #main folders
for (k in 43) {
  main_path = "/Volumes/EyeTrackUAV2/RAW_DATA/RAW_DATA_EyeTracker_CoordSystem"
  setwd(main_path)
  event_folders = list.dirs('.', recursive = F); #event (43) folders
  event_folders = event_folders[2:length(event_folders)]
  for (i in 1:length(event_folders)) {
    ext1 = event_folders[i]
    event_path = paste(getwd(), substr(ext1, 2, nchar(ext1)), sep = "")
    setwd(event_path)
    for (j in 1) { 
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
        
        if (s_gaze == "There were no fixations or saccades classified, probably data quality of this particpant is very low") { #exclude poor quality data
          next
        }
        
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
        alg_data = c(alg_fix_count, alg_fix_dur_avg, alg_sac_count, alg_sac_dur_avg)
        
        #Getting the subject's fixation and saccade groundtruth values
        
        #Fixation
        #Get to the right folder of 30 csvs
        setwd("/Volumes/EyeTrackUAV2/FIXATIONS")
        event_folders_fix = list.dirs('.', recursive = F); #event (43) folders
        event_folders_fix = event_folders_fix[2:length(event_folders_fix)]
        ext_fix1 = event_folders_fix[i]
        ext_fix1 = substr(ext_fix1, 2, nchar(ext_fix1))
        ext_fix2 = paste(ext_fix1,"_FV", sep = "")
        event_path_fix = (paste(getwd(), ext_fix1, ext_fix2, "/Binocular", sep = "")) #Change Binocular to Dominant when doing Dominant data
        setwd(event_path_fix)
        file_list_fix = list.files(getwd(), pattern="*.csv");
        
        #Choose the respective folder that matches the subject from the algorithm 
        eye_data_fix = read.csv(file_list_fix[l], header = FALSE)
        fix_count = length(eye_data[,1])
        fix_dur_avg = mean(eye_data[,6], na.rm = T)
        ground_data = c(fix_count, fix_dur_avg)
        
        #Saccade
        #Get to the right folder of 30 csvs
        setwd("/Volumes/EyeTrackUAV2/SACCADES")
        event_folders_sac = list.dirs('.', recursive = F); #event (43) folders
        event_folders_sac = event_folders_sac[2:length(event_folders_sac)]
        ext_sac1 = event_folders_sac[i]
        ext_sac1 = substr(ext_sac1, 2, nchar(ext_sac1))
        ext_sac2 = paste(ext_sac1,"_FV", sep = "")
        event_path_sac = (paste(getwd(), ext_sac1, ext_sac2, "/Binocular", sep = "")) #Change Binocular to Dominant when doing Dominant data
        setwd(event_path_sac)
        file_list_sac = list.files(getwd(), pattern="*.csv");
        
        #Choose the respective folder that matches the subject from the algorithm 
        eye_data_sac = read.csv(file_list_sac[l], header = FALSE)
        sac_count = length(eye_data_sac[,1])
        sac_dur_avg = mean(eye_data_sac[,6], na.rm = T)
        ground_data = c(ground_data, sac_count, sac_dur_avg)
        
        
        #Comparing algorithm and groundtruth values
        squared_differences = (ground_data - alg_data)^2
        
        SD_event_data = rbind(SD_event_data, squared_differences) #30 values of each subject's squared diff values for four metrics 
        names(SD_event_data) = c("SD fixation count", "SD fixation duration", "SD saccade count", "SD saccade duration") 
      }
      
    for (i in 1:4) {
      RMSE_value = RMSE(SD_event_data, i)
      RMSE_values = c(RMSE_values, RMSE_value)
    }
  
    RMSE_data = rbind(RMSE_data, RMSE_values)
    }
    
    SD_event_data = data.frame()
    RMSE_values = c()
    alg_data = data.frame()
    
    setwd("/Volumes/EyeTrackUAV2/RAW_DATA/RAW_DATA_EyeTracker_CoordSystem")
  }
  
  #reset_path = "/Volumes/EyeTrackUAV2/RAW_DATA/RAW_DATA_EyeTracker_CoordSystem"  # paste in your directory pathway where the folder is - can find this by doing getwd() of the directory after setting it earlier
  setwd(main_path)
}
  

#ground_truth = ground_truth%>%
#  rename(
#    name = paste.ext1.,
#    fix_count = mean.avg_data.fix_count.,
#    fix_duration = mean.avg_data.fix_dur_avg.,
#    sac_count = mean.avg_data.sac_count.,
#    sac_duration = mean.avg_data.sac_dur_avg.
#  )%>%
#  arrange(name)

names(RMSE_data) <- c("Filename", "RMSE fixation count", "RMSE fixation duration", "RMSE saccade count", "RMSE saccade duration") 

write.csv(RMSE_data,"/Users/aartheebaskaran/Desktop/RMASE.csv", row.names = FALSE) #change this to your where you want to output the csv with the same title

