#Binocular/Count/Gazepath 
setwd("C:/Users/nauti/Documents/Esurgi")
binocular_data<- read.csv("UAV223.csv")

#Installs
##install.packages("psych")

#install.packages("lme4",
#                repos=c("http://lme4.r-forge.r-project.org/repos",
#                     getOption("repos")[["CRAN"]]))
#libraries 

library(psych)
library("irr")

#Binocular/Count/Gazepath
names(binocular_data)
print (binocular_data)
entireCG_set<-data.frame(binocular_data[,c(2,11)])
entireCG_dataprint<-ICC(
  entireCG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCG_dataprint)


#Binocular/Count/Dispersion
entireCD_set<-data.frame(binocular_data[,c(3,11)])

entireCD_dataprint<-ICC(
  entireCD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCD_dataprint)
#Binocular/Count/Velocity 
entireCV_set<-data.frame(binocular_data[,c(4,11)])

entireCV_dataprint<-ICC(
  entireCV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCV_dataprint)
#Binocular/Average Duration/Gazepath

entireADG_set<-data.frame(binocular_data[,c(5,12)])

entireADG_dataprint<-ICC(
  entireADG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireADG_dataprint)

#Binocular/Average Duration/Dispersion

entireADD_set<-data.frame(binocular_data[,c(6,12)])

entireADD_dataprint<-ICC(
  entireADD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireADD_dataprint)

#Binocular/Average Duration/Velocity

entireADV_set<-data.frame(binocular_data[,c(7,12)])

entireADV_dataprint<-ICC(
  entireADV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireADV_dataprint)

#Binocular/Cumulative Duration/Gazepath

entireCDG_set<-data.frame(binocular_data[,c(8,13)])

entireCDG_dataprint<-ICC(
  entireCDG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCDG_dataprint)

#Binocular/Cumulative Duration/Dispersion

entireCDD_set<-data.frame(binocular_data[,c(9,13)])

entireCDD_dataprint<-ICC(
  entireCDD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCDD_dataprint)

#Binocular/Cumulative Duration/Velocity 

entireCDV_set<-data.frame(binocular_data[,c(10,13)])

entireCDV_dataprint<-ICC(
  entireCDV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireCDV_dataprint)


#Dominant

dominant_data<- read.csv("UAV224.csv")
#Dominant/Count/Gazepath

entireDCG_set<-data.frame(dominant_data[3:1288,c(17,26)])
print(entireDCG_set)
entireDCG_dataprint<-ICC(
  entireDCG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCG_dataprint)
#Dominant/Count/Dispersion
entireDCD_set<-data.frame(dominant_data[3:1290,c(18,26)])

entireDCD_dataprint<-ICC(
  entireDCD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCD_dataprint)
#Dominant/Count/Velocity 
entireDCV_set<-data.frame(dominant_data[3:1290,c(19,26)])

entireDCV_dataprint<-ICC(
  entireDCV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCV_dataprint)
#Dominant/Average Duration/Gazepath

entireDADG_set<-data.frame(dominant_data[3:1290,c(20,27)])

entireDADG_dataprint<-ICC(
  entireDADG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDADG_dataprint)

#Dominant/Average Duration/Dispersion

entireDADD_set<-data.frame(dominant_data[3:1290,c(21,27)])

entireDADD_dataprint<-ICC(
  entireDADD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDADD_dataprint)

#Dominant/Average Duration/Velocity

entireDADV_set<-data.frame(dominant_data[3:1290,c(22,27)])

entireDADV_dataprint<-ICC(
  entireDADV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDADV_dataprint)

#Dominant/Cumulative Duration/Gazepath

entireDCDG_set<-data.frame(dominant_data[3:1290,c(23,28)])

entireDCDG_dataprint<-ICC(
  entireDCDG_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCDG_dataprint)

#Dominant/Cumulative Duration/Dispersion

entireDCDD_set<-data.frame(dominant_data[3:1290,c(24,28)])

entireDCDD_dataprint<-ICC(
  entireDCDD_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCDD_dataprint)

#Dominant/Cumulative Duration/Velocity 

entireDCDV_set<-data.frame(dominant_data[3:1290,c(25,28)])

entireDCDV_dataprint<-ICC(
  entireDCDV_set, missing = TRUE, alpha = .05,lmer=TRUE, check.keys = FALSE
)
print(entireDCDV_dataprint)