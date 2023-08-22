### RHF COVER 2019 ###

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
rhf19data<-read.csv("rhf_2019_cover.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(rhf19data$Species)

#manually remove dodgy entries
#remove notes column
rhf19data<-rhf19data%>%filter(Species!="moss"&!grepl("poo",Species)&!grepl("grass",Species)&
                                !grepl("soil",Species)&!grepl("onion",Species)&!grepl("log",Species)&
                                Species!="rocks"&Species!="organic matter"&
                                Species!="un-ided small plant"&Species!="moss"&
                                Species!="light purple flower"&
                                Species!="new tiny plant"&
                                Species!="woody little spray of leaves"&
                                Species!="gray lichen"&Species!="light green soldier lichen"&
                                Species!="jagged edged rosette"&
                                Species!="thin leaved rough plant"&
                                Species!="alternate leaved plant"&
                                Species!="little green plant"&Species!="tall green thin seed"&
                                Species!="pale fuzzy many dissected leaf"&
                                Species!="lichen"&Species!="apiaceae tall white umbel"&
                                Species!="little woody sprout"&Species!="new white flower feather leaf"&
                                Species!="bark"&Species!="new heart leaved vine"&Species!="thick stalk"&
                                Species!="dried star plant"&Species!="woody shrub simple leaves"&
                                Species!="thistle"&Species!="Delphinium/Geranium sp.")%>%
  select(Date,Recorder_1,Plot_id,Quadrant,Species,Cover)%>%
  mutate(Species = case_when(str_detect(Species, "chickweed") ~ "Stellaria sp.",TRUE~Species))%>%
  mutate(Species = case_when(str_detect(Species, "brassica") ~ "Brassicaceae.",TRUE~Species))%>%
  mutate(Species = case_when(str_detect(Species, "lily") ~ "Liliaceae.",TRUE~Species))%>%
  mutate(Species = case_when(str_detect(Species, "asteraceae") ~ "Asteraceae.",TRUE~Species))%>%
  mutate(Species = case_when(str_detect(Species, "Erigeron sp.") ~ "Erigeron sp.",TRUE~Species))

#rename dodgy names
rhf19data$Species[rhf19data$Species=="carex greens?"]<-"Carex sp."
rhf19data$Species[rhf19data$Species=="new aster center flwr rosette"|rhf19data$Species=="tongue leaved aster"|
                    rhf19data$Species=="very thin leaved aster"]<-"Aster sp."
rhf19data$Species[rhf19data$Species=="pea"]<-"Fabaceae sp."
rhf19data$Species[rhf19data$Species=="clover"]<-"Trifolium repens"

#potentially Asteraceae/Liliaceae, etc. should be removed

#rename bad spelling
rhf19data$Species[rhf19data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
rhf19data$Species[rhf19data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
rhf19data$Species[rhf19data$Species=="Vicia fava"]<-"Vicia faba"
rhf19data$Species[rhf19data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
rhf19data$Species[rhf19data$Species=="Succisa pratensid"]<-"Succisa pratensis"
rhf19data$Species[rhf19data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
rhf19data$Species[rhf19data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
rhf19data$Species[rhf19data$Species=="Trifolium campastre"]<-"Trifolium campestre"
rhf19data$Species[rhf19data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
rhf19data$Species[rhf19data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
rhf19data$Species[rhf19data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
rhf19data$Species[rhf19data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
rhf19data$Species[rhf19data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
rhf19data$Species[rhf19data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
rhf19data$Species[rhf19data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
rhf19data$Species[rhf19data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
rhf19data$Species[rhf19data$Species=="Rubus ideaus"]<-"Rubus idaeus"
rhf19data$Species[rhf19data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
rhf19data$Species[rhf19data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
rhf19data$Species[rhf19data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
rhf19data$Species[rhf19data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
rhf19data$Species[rhf19data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
rhf19data$Species[rhf19data$Species=="Circium undulatum"]<-"Cirsium undulatum"
rhf19data$Species[rhf19data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
rhf19data$Species[rhf19data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
rhf19data$Species[rhf19data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
rhf19data$Species[rhf19data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
rhf19data$Species[rhf19data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
rhf19data$Species[rhf19data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
rhf19data$Species[rhf19data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
rhf19data$Species[rhf19data$Species=="Aster engelmanii"]<-"Aster engelmannii"
rhf19data$Species[rhf19data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
rhf19data$Species[rhf19data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
rhf19data$Species[rhf19data$Species=="Lomatium greyi"]<-"Lomatium grayi"
rhf19data$Species[rhf19data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
rhf19data$Species[rhf19data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
rhf19data$Species[rhf19data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
rhf19data$Species[rhf19data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
rhf19data$Species[rhf19data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
rhf19data$Species[rhf19data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
rhf19data$Species[rhf19data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
rhf19data$Species[rhf19data$Species=="Holcus molis"]<-"Holcus mollis"
rhf19data$Species[rhf19data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
rhf19data$Species[rhf19data$Species=="Luzulla campestris"]<-"Luzula campestris"
rhf19data$Species[rhf19data$Species=="Fetusca rubra"]<-"Festuca rubra"
rhf19data$Species[rhf19data$Species=="Ranculus repens"]<-"Ranunculus repens"
rhf19data$Species[rhf19data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
rhf19data$Species[rhf19data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
rhf19data$Species[rhf19data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
rhf19data$Species[rhf19data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
rhf19data$Species[rhf19data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
rhf19data$Species[rhf19data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
rhf19data$Species[rhf19data$Species=="Circium arvense"]<-"Cirsium arvense"
rhf19data$Species[rhf19data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
rhf19data$Species[rhf19data$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(rhf19data$Species)

#fix date column format
rhf19data$Date<-as.Date(rhf19data$Date)
#add year columns
rhf19data$Year <- as.numeric(format(rhf19data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(rhf19data, 'rhf19.xlsx')
