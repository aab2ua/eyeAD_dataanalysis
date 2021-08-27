#Lines that you may need to edit before running regarding folder locations: lines 24, 142, 165 

#Lines you need to check everytime before running: lines 29, 200

#Run once each session 
library(gazepath) #has code for gazepath algorithms 
library(dplyr)

#Need to have this in environment before running algorithm
names = c("height", "width", "resx", "resy")
specs = c(294.46,	527.04,	1920,	1080)
screen = data.frame(names, specs)

ground_data <- data.frame()
ground_data_vector = vector()
final_data = data.frame()
alg_data = data.frame()
alg_data_vector = vector()
count_files = 0 

event_count = 0
csv_count = 0

main_path = "/Volumes/EyeTrackUAV2/RAW_DATA/RAW_DATA_EyeTracker_CoordSystem" #Change based on where your binocular raw data is located 
setwd(main_path)
event_folders = list.dirs('.', recursive = F) #event (43) folders

#change the range (ex - 1:5) based on what folders you want to compute - recommend doing small ranges so program doesn't crash. It is inclusive of 1 to 5
for (i in 1:5) {
  csv_count = 0
  event_count = event_count + 1
  ext1 = event_folders[i]
  event_path = paste(getwd(), substr(ext1, 2, nchar(ext1)), sep = "")
  setwd(event_path)
  
  file_list = list.files(getwd(), pattern="*.csv")
  for (l in seq(1, length(file_list), 2)){ 
    setwd(event_path)
    alg_file = file_list[l]
    eye_data = read.csv(alg_file, header = FALSE)
    
    eye_data = eye_data%>%
      mutate(trial = 1)%>%
      mutate(distance = 1000)
    
    #GAZEPATH
    alg_results_gaze = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                                width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "gazepath",
                                samplerate = 1000, thres_dur = 80)
    s_gaze = summary(alg_results_gaze)
    
    #if (s_gaze == "There were no fixations or saccades classified, probably data quality of this particpant is very low") { #exclude poor quality data
    #  next
    #}
    
    #Getting fixation metrics:
    fix_filtered_gaze = s_gaze%>%
      filter(Value == "f")
    
    fix_count_set_gaze = fix_filtered_gaze[1]
    alg_fix_count_gaze = length(fix_count_set_gaze[,1])
    alg_fix_dur_avg_gaze = mean(fix_filtered_gaze$Duration, na.rm = T)
    alg_fix_dur_cum_gaze = sum(fix_filtered_gaze$Duration)
    
    # #Getting saccade metrics:
    sac_filtered_gaze = s_gaze%>%
      filter(Value == "s")
    
    sac_count_set_gaze = sac_filtered_gaze[1]
    alg_sac_count_gaze = length(sac_count_set_gaze[,1])
    alg_sac_dur_avg_gaze = mean(sac_filtered_gaze$Duration, na.rm = T)
    alg_sac_dur_cum_gaze = sum(sac_filtered_gaze$Duration)

    #DISPERSION
    alg_results_dis = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                               width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "dispersion",
                               samplerate = 1000, thres_dur = 80)
    s_dis = summary(alg_results_dis)
    
    #Getting fixation metrics:
    fix_filtered_dis = s_dis%>%
      filter(Value == "f")
    
    fix_count_set_dis = fix_filtered_dis[1]
    alg_fix_count_dis = length(fix_count_set_dis[,1])
    alg_fix_dur_avg_dis = mean(fix_filtered_dis$Duration, na.rm = T)
    alg_fix_dur_cum_dis = sum(fix_filtered_dis$Duration)

    # #Getting saccade metrics:
    sac_filtered_dis = s_dis%>%
      filter(Value == "s")
    
    sac_count_set_dis = sac_filtered_dis[1]
    alg_sac_count_dis = length(sac_count_set_dis[,1])
    alg_sac_dur_avg_dis = mean(sac_filtered_dis$Duration, na.rm = T) / 10
    alg_sac_dur_cum_dis = sum(sac_filtered_dis$Duration)

    #VELOCITY
    alg_results_vel = gazepath(eye_data, x1 = 8, y1 = 9, d1 = 12, trial = 11, height_mm = screen[1,2],
                               width_mm = screen[2,2], width_px = screen[3,2], height_px = screen[4,2], method = "velocity",
                               samplerate = 1000, thres_dur = 80)
    s_vel = summary(alg_results_vel)
    
    #Getting fixation metrics:
    fix_filtered_vel = s_vel%>%
      filter(Value == "f")
    
    fix_count_set_vel = fix_filtered_vel[1]
    alg_fix_count_vel = length(fix_count_set_vel[,1])
    alg_fix_dur_avg_vel = mean(fix_filtered_vel$Duration, na.rm = T)
    alg_fix_dur_cum_vel = sum(fix_filtered_vel$Duration)
    
    # #Getting saccade metrics:
    sac_filtered_vel = s_vel%>%
      filter(Value == "s")
    
    sac_count_set_vel = sac_filtered_vel[1]
    alg_sac_count_vel = length(sac_count_set_vel[,1])
    alg_sac_dur_avg_vel = mean(sac_filtered_vel$Duration, na.rm = T)
    alg_sac_dur_cum_vel = sum(sac_filtered_vel$Duration)
    
    #adding to averaging data set
    alg_data_vector = c(alg_file, alg_fix_count_gaze, alg_fix_count_dis, alg_fix_count_vel,
                 alg_fix_dur_avg_gaze, alg_fix_dur_avg_dis, alg_fix_dur_avg_vel,
                 alg_fix_dur_cum_gaze, alg_fix_dur_cum_dis, alg_fix_dur_cum_vel
                 alg_sac_count_gaze, alg_sac_count_dis, alg_sac_count_vel,
                 alg_sac_dur_avg_gaze, alg_sac_dur_avg_dis, alg_sac_dur_avg_vel
                 alg_sac_dur_cum_gaze, alg_sac_dur_cum_dis, alg_sac_dur_cum_vel)
    
    if (l == 1) {
      alg_data = alg_data_vector
    } 
    
    else {
      alg_data = cbind(alg_data, alg_data_vector)
    }  
    
    #Getting the subject's fixation and saccade groundtruth values
    
    #Fixation
    #Getting to the right folder of 30 csvs
    setwd("/Volumes/EyeTrackUAV2/FIXATIONS") #Change based on where the folder location is 
    event_folders_fix = list.dirs('.', recursive = F)
    ext_fix1 = event_folders_fix[i]
    ext_fix1 = substr(ext_fix1, 2, nchar(ext_fix1))
    ext_fix2 = paste(ext_fix1,"_FV", sep = "")
    event_path_fix = paste(getwd(), ext_fix1, ext_fix2, "/Binocular", sep = "") 
    setwd(event_path_fix)
    file_list_fix = list.files(getwd(), pattern="*.csv")
    
    #Choose the respective folder that matches the subject from the algorithm 
    fix_file = file_list_fix[(l+1)/2] # change index to l if dominant data
    if(substr(alg_file,1,length(alg_file)-4) != substr(fix_file,1,length(alg_file)-4)){
      stop(c(alg_file,fix_file))
    }
    eye_data_fix = read.csv(fix_file, header = FALSE)
    fix_count = length(eye_data_fix[,1])
    fix_dur_avg = mean(eye_data_fix[,6], na.rm = T)
    fix_dur_cum = sum(eye_data_fix[,6])
    
    ground_data_vector = c(alg_file, fix_count, fix_dur_avg, fix_dur_cum)
    
    #Saccade
    #Get to the right folder of 30 csvs
    setwd("/Volumes/EyeTrackUAV2/SACCADES") #Change based on where the folder location is 
    event_folders_sac = list.dirs('.', recursive = F)
    ext_sac1 = event_folders_sac[i]
    ext_sac1 = substr(ext_sac1, 2, nchar(ext_sac1))
    ext_sac2 = paste(ext_sac1,"_FV", sep = "")
    event_path_sac = paste(getwd(), ext_sac1, ext_sac2, "/Binocular", sep = "") 
    setwd(event_path_sac)
    file_list_sac = list.files(getwd(), pattern="*.csv");
    
    #Choose the respective folder that matches the subject from the algorithm
    sac_file = file_list_sac[(l+1)/2] 
    if(substr(alg_file,1,length(alg_file)-4) != substr(sac_file,1,length(alg_file)-4)){
      stop(c(alg_file,sac_file))
    }
    
    eye_data_sac = read.csv(sac_file, header = FALSE)
    sac_count = length(eye_data_sac[,1])
    sac_dur_avg = mean(eye_data_sac[,6], na.rm = T)
    sac_dur_cum = sum(eye_data_sac[,6])
    
    ground_data_vector = c(ground_data_vector, sac_count, sac_dur_avg, sac_dur_cum)

  }
  
  #adding to averaging data set
  if (l == 1) {
    ground_data = ground_data_vector
  }
  else {
    ground_data = cbind(ground_data, ground_data_vector)
  }
 }
  
  alg_data = t(alg_data)
  ground_data = t(ground_data)
  full_event_data = cbind(alg_data, ground_data)
  
  if (i == 1) {    #put the minimum of the range for the for loop
    final_data = full_event_data
  }
  else {
    final_data = rbind(final_data, full_event_data)
  } 

  full_event_data = data.frame()
  alg_data = data.frame()
  alg_data_vector = vector()
  ground_data = data.frame()
  ground_data_vector = vector()
  
  setwd(main_path)
}

final_data = data.frame(final_data)

names(final_data) <- c("File name", 
                       "Gaze fixation count", "Disp. fixation count", "Vel. fixation count",
                       "Gaze fix avg. dur", "Disp fix avg. dur", "Vel fix avg. dur",
                       "Gaze fix cum. dur", "Disp fix cum. dur", "Vel fix cum. dur",
                       "Gaze saccade count", "Disp saccade count", "Vel saccade count",
                       "Gaze sac avg, dur", "Disp sac avg, dur", "Vel sac avg, dur",
                       "Gaze sac cum. dur", "Disp sac cum. dur", "Vel sac cum. dur",
                       "File name", 
                       "GT fix count", "GT fix avg. dur", "GT fix cum. dur", "GT sac count", "GT sac avg. dur", "GT sac cum. dur")
  
write.csv(final_data,"//Users/aartheebaskaran/Desktop/Binocular_data.csv", row.names = FALSE) #change this to your where you want to output the csv with the correct title
