### ORDU URBAN 2021 ###

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
ordu21data<-read.csv("plant-datasheet_FF_Ordu2.csv")

## DATA CLEANING ##

#see list of 'species' names
unique(ordu21data$Species)

#manually remove dodgy entries
#remove notes column
ordu21data<-ordu21data%>%filter(Species!="soil")%>%select(ï..Date,Recorder,Site,Plot,Quadrant,Species,Cover)


#rename bad spelling
ordu21data$Species[ordu21data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
ordu21data$Species[ordu21data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
ordu21data$Species[ordu21data$Species=="Vicia fava"]<-"Vicia faba"
ordu21data$Species[ordu21data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
ordu21data$Species[ordu21data$Species=="Succisa pratensid"]<-"Succisa pratensis"
ordu21data$Species[ordu21data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
ordu21data$Species[ordu21data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
ordu21data$Species[ordu21data$Species=="Trifolium campastre"]<-"Trifolium campestre"
ordu21data$Species[ordu21data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
ordu21data$Species[ordu21data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
ordu21data$Species[ordu21data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
ordu21data$Species[ordu21data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
ordu21data$Species[ordu21data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
ordu21data$Species[ordu21data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
ordu21data$Species[ordu21data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
ordu21data$Species[ordu21data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
ordu21data$Species[ordu21data$Species=="Rubus ideaus"]<-"Rubus idaeus"
ordu21data$Species[ordu21data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
ordu21data$Species[ordu21data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
ordu21data$Species[ordu21data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
ordu21data$Species[ordu21data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
ordu21data$Species[ordu21data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
ordu21data$Species[ordu21data$Species=="Circium undulatum"]<-"Cirsium undulatum"
ordu21data$Species[ordu21data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
ordu21data$Species[ordu21data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
ordu21data$Species[ordu21data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
ordu21data$Species[ordu21data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
ordu21data$Species[ordu21data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
ordu21data$Species[ordu21data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
ordu21data$Species[ordu21data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
ordu21data$Species[ordu21data$Species=="Aster engelmanii"]<-"Aster engelmannii"
ordu21data$Species[ordu21data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
ordu21data$Species[ordu21data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
ordu21data$Species[ordu21data$Species=="Lomatium greyi"]<-"Lomatium grayi"
ordu21data$Species[ordu21data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
ordu21data$Species[ordu21data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
ordu21data$Species[ordu21data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
ordu21data$Species[ordu21data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
ordu21data$Species[ordu21data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
ordu21data$Species[ordu21data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
ordu21data$Species[ordu21data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
ordu21data$Species[ordu21data$Species=="Holcus molis"]<-"Holcus mollis"
ordu21data$Species[ordu21data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
ordu21data$Species[ordu21data$Species=="Luzulla campestris"]<-"Luzula campestris"
ordu21data$Species[ordu21data$Species=="Fetusca rubra"]<-"Festuca rubra"
ordu21data$Species[ordu21data$Species=="Ranculus repens"]<-"Ranunculus repens"
ordu21data$Species[ordu21data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
ordu21data$Species[ordu21data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
ordu21data$Species[ordu21data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
ordu21data$Species[ordu21data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
ordu21data$Species[ordu21data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
ordu21data$Species[ordu21data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
ordu21data$Species[ordu21data$Species=="Circium arvense"]<-"Cirsium arvense"
ordu21data$Species[ordu21data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
ordu21data$Species[ordu21data$Species=="Poa trivalis"]<-"Poa trivialis"


#check list of species names
unique(ordu21data$Species)

#fix date column format
#fix date column format
ordu21data<-ordu21data %>% rename(Date=ï..Date)
ordu21data$Date<-as.Date(ordu21data$Date,"%d/%m/%Y")
#add year column
ordu21data$Year <- as.numeric(format(ordu21data$Date,'%Y'))


#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(ordu21data, 'ordu21.xlsx')
