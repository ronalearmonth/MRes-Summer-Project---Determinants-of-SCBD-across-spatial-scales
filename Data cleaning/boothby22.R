### BOOTHBY 2022 ###

rm(list=ls())

#load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)

#set wd
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

#import datasheet
boothby22data<-read.csv("boothby-plant-datasheet2.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(boothby22data$Species)

#manually remove dodgy entries
#remove notes column
boothby22data<-boothby22data%>%filter(Species!="dead leaves"&Species!="bare soil"&
                                Species!="stones"&Species!="stone"&Species!="shoot"&
                                Species!="senesced grass"&Species!=""&
                                Species!="sticks"&Species!="moss"&
                                Species!="dead"&Species!="Taraxacum / Crepis sp"&Species!="bark"&
                                Species!="bracken"&Species!="animal droppings"&
                                Species!="feather")%>%
  select(ï..Date,Recorder,Site,Block,Plot,Quadrant,Species,Cover)
#potentially Brassicaceae should be removed
boothby22data$Species[boothby22data$Species=="Carrot sp"]<-"Daucus sp"
boothby22data$Species[boothby22data$Species=="Rubus sp?"]<-"Rubus sp"
boothby22data$Species[boothby22data$Species=="Prunus sp.inosa"|boothby22data$Species=="Prunus sp.inosa?"]<-"Prunus spinosa"

#add dot after sp
boothby22data$Species<-gsub(" sp"," sp.",boothby22data$Species)

#check list of species names
unique(boothby22data$Species)

#fix date column format
boothby22data<-boothby22data %>% rename(Date=ï..Date)
boothby22data$Date<-as.Date(boothby22data$Date,"%d/%m/%Y")
#add year column
boothby22data$Year <- as.numeric(format(boothby22data$Date,'%Y'))

#group Group and Block together
boothby22data<-boothby22data%>%mutate(Plot=paste(Block,Plot,sep=""))%>%
  select(Year,Recorder,Plot,Quadrant,Species,Cover,Site)

#remove * from plot names
boothby22data$Plot<-str_replace(boothby22data$Plot, '\\*', '')

#save as new excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(boothby22data, 'boothby22.xlsx')

