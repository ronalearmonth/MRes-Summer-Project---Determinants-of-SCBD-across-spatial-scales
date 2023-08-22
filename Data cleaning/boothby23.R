### BOOTHBY DATA CLEANING ###

rm(list=ls())

library(dplyr)
library(lubridate)

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

boothby23<-read.csv("Boothby_2023.csv")

boothby23<-boothby23%>% rename(Date=ï..Date)
boothby23$Date<-as.Date(dmy(boothby23$Date))

boothby23<-boothby23%>%
  select(Year,Recorder,Plot,Quadrant,Species,Cover,Site)%>%
  filter(Species!=""&Species!="sticks"&Species!="Bare ground"&Species!="Dead wood"&
           Species!="Dead twigs"&Species!="Leaf litter")

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.csv(boothby23, 'boothby23.csv')
