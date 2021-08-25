library(gazepath)

library(dplyr)



#Need to have this in environment before running algorithm

names = c("height", "width", "resx", "resy")

specs = c(294.46,	527.04,	1920,	1080)

screen = data.frame(names, specs)



final_data = data.frame()

alg_data = data.frame()

alg_data_vector = vector()

count_files = 0 



event_count = 0

csv_count = 0



main_path = "C:/Users/aab2ua/Desktop/RAW_DATA_OrigRes_DomEye" #change when calculating saccade results

setwd(main_path)

event_folders = list.dirs('.', recursive = F) #event (43) folders



#change the range (ex:1:5) based on what folders you want to fill in - recommend doing small ranges so program doesn't crash.

for (i in 3:15) { #265 line 
  
  csv_count = 0
  
  event_count = event_count + 1
  
  ext1 = event_folders[i]
  
  event_path1 = paste(getwd(), substr(ext1, 2, nchar(ext1)), sep = "")
  
  event_path2 = paste(ext1,"_FV", sep = "") # uncomment for dominant data
  
  event_path2 = substr(event_path2, 2, nchar(event_path2)) 
  
  event_path = paste(event_path1, event_path2, sep = "")
  
  setwd(event_path)
  
  
  
  file_list = list.files(getwd(), pattern="*.csv")
  
  for (l in seq(1, 30, 1)){ #iterate through every file for dominant data
    
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
    
    # next
    
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
                        
                        alg_fix_dur_cum_gaze, alg_fix_dur_cum_dis, alg_fix_dur_cum_vel,
                        
                        alg_sac_count_gaze, alg_sac_count_dis, alg_sac_count_vel,
    
                        alg_sac_dur_avg_gaze, alg_sac_dur_avg_dis, alg_sac_dur_avg_vel,

                        alg_sac_dur_cum_gaze, alg_sac_dur_cum_dis, alg_sac_dur_cum_vel)



      if (l == 1) {
        alg_data = alg_data_vector
      } 
      else {
        alg_data = cbind(alg_data, alg_data_vector)
      } 

    }
  
  
  
  
  alg_data = t(alg_data)
  
  if (i == 3) {
    final_data = alg_data
  }
  else {
    final_data = rbind(final_data, alg_data)
  } 
  
  
  alg_data = data.frame()
  
  alg_data_vector = vector()
  
  setwd(main_path)
  
}



final_data = data.frame(final_data)


names(final_data) <- c("File name",
                       
                       "Gaze fixation count", "Disp. fixation count", "Vel. fixation count",
                       
                       "Gaze fix avg. dur", "Disp fix avg. dur", "Vel fix avg. dur",
                       
                       "Gaze fix cum. dur", "Disp fix cum. dur", "Vel fix cum. dur",
                       
                       "Gaze saccade count", "Disp saccade count", "Vel saccade count",
                       
                       "Gaze sac avg, dur", "Disp sac avg, dur", "Vel sac avg, dur",
                       
                       "Gaze sac cum. dur", "Disp sac cum. dur", "Vel sac cum. dur")
                       
                    
setwd("C:/Users/aab2ua/Desktop")

write.csv(final_data,"final_data_Dom_3_15.csv", row.names = FALSE) #change this to your where you want to output the csv wit
