### SILWOOD 2023 DATA CLEANING ###

#-------------------------------------------------------------------------------

rm(list=ls())

#load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)

#set wd
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

#import datasheet
silwood23data<-read_excel("Silwood_2023.xlsx")

#-------------------------------------------------------------------------------

## DATA CLEANING ##

#see list of 'species' names
unique(silwood23data$Species)

#fix date column format
silwood23data$Date<-as.Date(silwood23data$Date)
#add year columns
silwood23data$Year <- as.numeric(format(silwood23data$Date,'%Y'))

silwood23data<-silwood23data%>%filter(Species!="Leaf litter"&Species!="Moss"&
                                        Species!="Bare ground"&Species!="Dead grass"&
                                        Species!="Dead leaves"&Species!="Dead twig"&
  Species!="Bare Ground"&Species!="Crepis spp.")%>%
  select(Year,Site,Plot,Quadrant,Species,Cover)

#-------------------------------------------------------------------------------

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.csv(silwood23data, 'silwood23.csv')




