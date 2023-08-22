### ENEZ FOREST 2021 ###

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
enez21data<-read.csv("plant-datasheet_FFcsv.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(enez21data$Species)

#manually remove dodgy entries
#remove notes column
enez21data<-enez21data%>%filter(Species!="moss"&Species!="soil + dry leaves (pine and oak)")%>%
  select(ï..Date,Recorder,Site,Plot,Quadrant,Species,Cover)

#fix spelling
enez21data$Species[enez21data$Species=="Querus sp."]<-"Quercus sp."
enez21data$Species[enez21data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
enez21data$Species[enez21data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
enez21data$Species[enez21data$Species=="Vicia fava"]<-"Vicia faba"
enez21data$Species[enez21data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
enez21data$Species[enez21data$Species=="Succisa pratensid"]<-"Succisa pratensis"
enez21data$Species[enez21data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
enez21data$Species[enez21data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
enez21data$Species[enez21data$Species=="Trifolium campastre"]<-"Trifolium campestre"
enez21data$Species[enez21data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
enez21data$Species[enez21data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
enez21data$Species[enez21data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
enez21data$Species[enez21data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
enez21data$Species[enez21data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
enez21data$Species[enez21data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
enez21data$Species[enez21data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
enez21data$Species[enez21data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
enez21data$Species[enez21data$Species=="Rubus ideaus"]<-"Rubus idaeus"
enez21data$Species[enez21data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
enez21data$Species[enez21data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
enez21data$Species[enez21data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
enez21data$Species[enez21data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
enez21data$Species[enez21data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
enez21data$Species[enez21data$Species=="Circium undulatum"]<-"Cirsium undulatum"
enez21data$Species[enez21data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
enez21data$Species[enez21data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
enez21data$Species[enez21data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
enez21data$Species[enez21data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
enez21data$Species[enez21data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
enez21data$Species[enez21data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
enez21data$Species[enez21data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
enez21data$Species[enez21data$Species=="Aster engelmanii"]<-"Aster engelmannii"
enez21data$Species[enez21data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
enez21data$Species[enez21data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
enez21data$Species[enez21data$Species=="Lomatium greyi"]<-"Lomatium grayi"
enez21data$Species[enez21data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
enez21data$Species[enez21data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
enez21data$Species[enez21data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
enez21data$Species[enez21data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
enez21data$Species[enez21data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
enez21data$Species[enez21data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
enez21data$Species[enez21data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
enez21data$Species[enez21data$Species=="Holcus molis"]<-"Holcus mollis"
enez21data$Species[enez21data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
enez21data$Species[enez21data$Species=="Luzulla campestris"]<-"Luzula campestris"
enez21data$Species[enez21data$Species=="Fetusca rubra"]<-"Festuca rubra"
enez21data$Species[enez21data$Species=="Ranculus repens"]<-"Ranunculus repens"
enez21data$Species[enez21data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
enez21data$Species[enez21data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
enez21data$Species[enez21data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
enez21data$Species[enez21data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
enez21data$Species[enez21data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
enez21data$Species[enez21data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
enez21data$Species[enez21data$Species=="Circium arvense"]<-"Cirsium arvense"
enez21data$Species[enez21data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
enez21data$Species[enez21data$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(enez21data$Species)

#fix date column format
enez21data<-enez21data %>% rename(Date=ï..Date)
enez21data$Date<-as.Date(enez21data$Date,"%d/%m/%Y")
#add year column
enez21data$Year <- as.numeric(format(enez21data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(enez21data, 'enez21.xlsx')
