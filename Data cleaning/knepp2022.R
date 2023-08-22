### KNEPP 2022 ###

rm(list=ls())

#load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)

#set wd
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

nell22<-read.csv("knepp-plant-datasheet2.csv")
#manually remove dodgy entries
#remove notes column
nell22<-nell22%>%filter(!grepl("dead",Species)&Species!="sticks"&
                          Species!="sapling"&Species!="moss"&
                          Species!="animal droppings"&Species!="pale green"&
                          Species!="CHECK"&Species!=""&
                          Species!="senesced flower"&Species!="hairy leaves"&
                          Species!="senesced grass"&
                          Species!="bare soil"&
                          Species!="roots"&Species!="feathers"&
                          Species!="grass"&Species!="lichen"&Species!="shoot"&
                          Species!="mud"&Species!="branches"&Species!="dry grass"&
                          Species!="leaves"&Species!="bracken"&Species!="water"&
                          Species!="Crepis sp OR Hieracium sp"&Species!="Mystery plant CHECK")%>%
  select(ï..Date,Recorder,Block,Group,Plot,Quadrant,Species,Cover)

nell22$Species[nell22$Species=="rose"]<-"Rosa sp"
nell22$Species[nell22$Species=="Vicia tetrasperma OR sativa"]<-"Vicia sp"
nell22$Species[nell22$Species=="Myosotis sylvatica / arvensis"]<-"Myosotis sp"

#add dot after sp for consistency
nell22$Species<-gsub(" sp"," sp.",nell22$Species)

#fix date column format
nell22<-nell22 %>% rename(Date=ï..Date)
nell22$Date<-as.Date(nell22$Date)

#add year column
nell22$Year <- as.numeric(format(nell22$Date,'%Y'))

#add site column
nell22$Site<-"Knepp"

#group Group and Block together
nell22<-nell22%>%mutate(Block=paste(Block,Group,sep=""))%>%
  select(Year,Recorder,Block,Plot,Quadrant,Species,Cover,Site)

#change nell blocks MNA and NNA to M and N
nell22$Block[nell22$Block=="MNA"]="M"
nell22$Block[nell22$Block=="NNA"]="N"

#remove *
nell22$Plot<-str_replace(nell22$Plot, '\\*', '')

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(nell22, 'knepp22.xlsx')
