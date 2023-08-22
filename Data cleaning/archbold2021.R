### ARCHBOLD, Florida 2021 ###

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
archbold21<-read_excel("2021 quad data.xlsx")

## DATA CLEANING ##

#data in horrible format
str(archbold21)

#select relevant data
archbold21<-archbold21 %>% select(Plot,Site_naming_scheme,Species,Quad_1,Quad_2,Quad_3,Quad_4)

#pivot
archbold21<-archbold21%>%pivot_longer(cols=c('Quad_1','Quad_2','Quad_3','Quad_4'),
                                  names_to="Quadrat",values_to="Cover")

#see list of 'species' names
unique(archbold21$Species)

#manually remove dodgy entries
#remove notes column
archbold21<-archbold21%>%filter(Species!="litter"&Species!="sand"&
                          Species!="Crustose lichen"&Species!="bare/muck"
                        &Species!="Bare ground")
archbold21$Species[archbold21$Species=="Fabaceae"]<-"Fabaceae sp."
#optional remove subspecies
archbold21$Species[archbold21$Species=="Commelina erecta var. angustifolia"]<-"Commelina erecta"
archbold21$Species[archbold21$Species=="Paronychia chartacea var. chartacea"]<-"Paronychia chartacea"
archbold21$Species[archbold21$Species=="Seymeria pectinata ssp. peninsularis"]<-"Seymeria pectinata"
archbold21$Species[archbold21$Species=="Andropogon virginicus var. 1"]<-"Andropogon virginicus"


#rename bad spelling
archbold21$Species[archbold21$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
archbold21$Species[archbold21$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
archbold21$Species[archbold21$Species=="Vicia fava"]<-"Vicia faba"
archbold21$Species[archbold21$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
archbold21$Species[archbold21$Species=="Succisa pratensid"]<-"Succisa pratensis"
archbold21$Species[archbold21$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
archbold21$Species[archbold21$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
archbold21$Species[archbold21$Species=="Trifolium campastre"]<-"Trifolium campestre"
archbold21$Species[archbold21$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
archbold21$Species[archbold21$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
archbold21$Species[archbold21$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
archbold21$Species[archbold21$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
archbold21$Species[archbold21$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
archbold21$Species[archbold21$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
archbold21$Species[archbold21$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
archbold21$Species[archbold21$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
archbold21$Species[archbold21$Species=="Rubus ideaus"]<-"Rubus idaeus"
archbold21$Species[archbold21$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
archbold21$Species[archbold21$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
archbold21$Species[archbold21$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
archbold21$Species[archbold21$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
archbold21$Species[archbold21$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
archbold21$Species[archbold21$Species=="Circium undulatum"]<-"Cirsium undulatum"
archbold21$Species[archbold21$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
archbold21$Species[archbold21$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
archbold21$Species[archbold21$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
archbold21$Species[archbold21$Species=="Comandra umbellatum"]<-"Comandra umbellata"
archbold21$Species[archbold21$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
archbold21$Species[archbold21$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
archbold21$Species[archbold21$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
archbold21$Species[archbold21$Species=="Aster engelmanii"]<-"Aster engelmannii"
archbold21$Species[archbold21$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
archbold21$Species[archbold21$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
archbold21$Species[archbold21$Species=="Lomatium greyi"]<-"Lomatium grayi"
archbold21$Species[archbold21$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
archbold21$Species[archbold21$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
archbold21$Species[archbold21$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
archbold21$Species[archbold21$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
archbold21$Species[archbold21$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
archbold21$Species[archbold21$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
archbold21$Species[archbold21$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
archbold21$Species[archbold21$Species=="Holcus molis"]<-"Holcus mollis"
archbold21$Species[archbold21$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
archbold21$Species[archbold21$Species=="Luzulla campestris"]<-"Luzula campestris"
archbold21$Species[archbold21$Species=="Fetusca rubra"]<-"Festuca rubra"
archbold21$Species[archbold21$Species=="Ranculus repens"]<-"Ranunculus repens"
archbold21$Species[archbold21$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
archbold21$Species[archbold21$Species=="Linium teniufolium"]<-"Linum tenuifolium"
archbold21$Species[archbold21$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
archbold21$Species[archbold21$Species=="Angelica silvestris"]<-"Angelica sylvestris"
archbold21$Species[archbold21$Species=="Engeron canadensis"]<-"Erigeron canadensis"
archbold21$Species[archbold21$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
archbold21$Species[archbold21$Species=="Circium arvense"]<-"Cirsium arvense"
archbold21$Species[archbold21$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
archbold21$Species[archbold21$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(archbold21$Species)

archbold21$Cover<-as.numeric(archbold21$Cover)
archbold21$Species<-as.factor(archbold21$Species)
archbold21$Quadrat<-as.factor(archbold21$Quadrat)

archbold21<-na.omit(archbold21)

#missing dates

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(archbold21, 'archbold21.xlsx')
