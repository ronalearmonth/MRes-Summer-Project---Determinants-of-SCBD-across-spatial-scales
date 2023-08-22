### KNEPP 2023 DATA CLEANING ###

rm(list=ls())

library(dplyr)

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

knepp23<-read.csv("Knepp_2023.csv")

knepp23<-knepp23%>% rename(Date=ï..Date)
knepp23$Date<-as.Date(dmy(knepp23$Date))

#add year column
knepp23$Year <- as.numeric(format(knepp23$Date,'%Y'))

#add site column
knepp23$Site<-"Knepp"

knepp23<-knepp23%>%
  select(Year,Recorder,Block,Plot,Quadrant,Species,Cover,Site)%>%
  filter(Species!=""&Species!="sticks"&Species!="Bare ground"&Species!="Dead wood"&
           Species!="Poo"&Species!="Dead twigs"&Species!="Moss"&
           Species!="Leaf litter")

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(knepp23, 'knepp23.xlsx')
