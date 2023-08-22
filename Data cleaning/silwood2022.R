### SILWOOD 2022 ###

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
silwood22data<-read.csv("plant_cover_2022.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(silwood22data$Species)

#manually remove dodgy entries
#remove notes column
silwood22data<-silwood22data%>%filter(Species!="moss"&Species!="leaf litter "&
                                        Species!="soil "&
                                Species!="wood "&Species!="dead grass"&
                                  Species!="dry grass "&
                                Species!="mud "&Species!=""&Species!="moss "&
                                  Species!="dried grass ")%>%
  select(ï..Date,Recorder,Site,Plot,Quadrant,Species,Cover)

#rename bad spelling
silwood22data$Species[silwood22data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
silwood22data$Species[silwood22data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
silwood22data$Species[silwood22data$Species=="Vicia fava"]<-"Vicia faba"
silwood22data$Species[silwood22data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
silwood22data$Species[silwood22data$Species=="Succisa pratensid"]<-"Succisa pratensis"
silwood22data$Species[silwood22data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
silwood22data$Species[silwood22data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
silwood22data$Species[silwood22data$Species=="Trifolium campastre"]<-"Trifolium campestre"
silwood22data$Species[silwood22data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
silwood22data$Species[silwood22data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
silwood22data$Species[silwood22data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
silwood22data$Species[silwood22data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
silwood22data$Species[silwood22data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
silwood22data$Species[silwood22data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
silwood22data$Species[silwood22data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
silwood22data$Species[silwood22data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
silwood22data$Species[silwood22data$Species=="Rubus ideaus"]<-"Rubus idaeus"
silwood22data$Species[silwood22data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
silwood22data$Species[silwood22data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
silwood22data$Species[silwood22data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
silwood22data$Species[silwood22data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
silwood22data$Species[silwood22data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
silwood22data$Species[silwood22data$Species=="Circium undulatum"]<-"Cirsium undulatum"
silwood22data$Species[silwood22data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
silwood22data$Species[silwood22data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
silwood22data$Species[silwood22data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
silwood22data$Species[silwood22data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
silwood22data$Species[silwood22data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
silwood22data$Species[silwood22data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
silwood22data$Species[silwood22data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
silwood22data$Species[silwood22data$Species=="Aster engelmanii"]<-"Aster engelmannii"
silwood22data$Species[silwood22data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
silwood22data$Species[silwood22data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
silwood22data$Species[silwood22data$Species=="Lomatium greyi"]<-"Lomatium grayi"
silwood22data$Species[silwood22data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
silwood22data$Species[silwood22data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
silwood22data$Species[silwood22data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
silwood22data$Species[silwood22data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
silwood22data$Species[silwood22data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
silwood22data$Species[silwood22data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
silwood22data$Species[silwood22data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
silwood22data$Species[silwood22data$Species=="Holcus molis"]<-"Holcus mollis"
silwood22data$Species[silwood22data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
silwood22data$Species[silwood22data$Species=="Luzulla campestris"]<-"Luzula campestris"
silwood22data$Species[silwood22data$Species=="Fetusca rubra"]<-"Festuca rubra"
silwood22data$Species[silwood22data$Species=="Ranculus repens"]<-"Ranunculus repens"
silwood22data$Species[silwood22data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
silwood22data$Species[silwood22data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
silwood22data$Species[silwood22data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
silwood22data$Species[silwood22data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
silwood22data$Species[silwood22data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
silwood22data$Species[silwood22data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
silwood22data$Species[silwood22data$Species=="Circium arvense"]<-"Cirsium arvense"
silwood22data$Species[silwood22data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
silwood22data$Species[silwood22data$Species=="Poa trivalis"]<-"Poa trivialis"


#check list of species names
unique(silwood22data$Species)

#fix date column format
silwood22data<-silwood22data %>% rename(Date=ï..Date)
silwood22data$Date<-as.Date(silwood22data$Date,"%d/%m/%Y")
silwood22data$Date<-silwood22data$Date %m+% years(2000) #convert from ddmmyy to ddmmyyyy   
#add year column
silwood22data$Year <- as.numeric(format(silwood22data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(silwood22data, 'silwood22.xlsx')
