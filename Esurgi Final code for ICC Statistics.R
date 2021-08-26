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
