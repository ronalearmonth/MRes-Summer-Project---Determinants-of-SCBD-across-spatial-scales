### SILWOOD COVER 2021 ###

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
silwood21data<-read.csv("silwood-cover-2021.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(silwood21data$Species)

#short list, therefore manually remove dodgy entries
#change oak (seedling) and oak (leaf-litter) to Quercus spp
#remove notes column
silwood21data<-silwood21data%>%filter(Species!="fluffy grass"&Species!="Opposite-alternates"&
                                      Species!="long grass"&Species!="moss (dying?)"&
                                        Species!="brain coral"&Species!="Pratensis-like")%>%
  mutate(Species = case_when(str_detect(Species, "oak") ~ "Quercus sp.",TRUE~Species))%>%
  select(Date,Recorder,Site,Quadrant,Species,Cover)


#rename bad spelling
silwood21data$Species[silwood21data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
silwood21data$Species[silwood21data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
silwood21data$Species[silwood21data$Species=="Vicia fava"]<-"Vicia faba"
silwood21data$Species[silwood21data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
silwood21data$Species[silwood21data$Species=="Succisa pratensid"]<-"Succisa pratensis"
silwood21data$Species[silwood21data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
silwood21data$Species[silwood21data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
silwood21data$Species[silwood21data$Species=="Trifolium campastre"]<-"Trifolium campestre"
silwood21data$Species[silwood21data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
silwood21data$Species[silwood21data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
silwood21data$Species[silwood21data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
silwood21data$Species[silwood21data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
silwood21data$Species[silwood21data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
silwood21data$Species[silwood21data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
silwood21data$Species[silwood21data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
silwood21data$Species[silwood21data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
silwood21data$Species[silwood21data$Species=="Rubus ideaus"]<-"Rubus idaeus"
silwood21data$Species[silwood21data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
silwood21data$Species[silwood21data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
silwood21data$Species[silwood21data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
silwood21data$Species[silwood21data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
silwood21data$Species[silwood21data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
silwood21data$Species[silwood21data$Species=="Circium undulatum"]<-"Cirsium undulatum"
silwood21data$Species[silwood21data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
silwood21data$Species[silwood21data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
silwood21data$Species[silwood21data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
silwood21data$Species[silwood21data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
silwood21data$Species[silwood21data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
silwood21data$Species[silwood21data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
silwood21data$Species[silwood21data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
silwood21data$Species[silwood21data$Species=="Aster engelmanii"]<-"Aster engelmannii"
silwood21data$Species[silwood21data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
silwood21data$Species[silwood21data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
silwood21data$Species[silwood21data$Species=="Lomatium greyi"]<-"Lomatium grayi"
silwood21data$Species[silwood21data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
silwood21data$Species[silwood21data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
silwood21data$Species[silwood21data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
silwood21data$Species[silwood21data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
silwood21data$Species[silwood21data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
silwood21data$Species[silwood21data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
silwood21data$Species[silwood21data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
silwood21data$Species[silwood21data$Species=="Holcus molis"]<-"Holcus mollis"
silwood21data$Species[silwood21data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
silwood21data$Species[silwood21data$Species=="Luzulla campestris"]<-"Luzula campestris"
silwood21data$Species[silwood21data$Species=="Fetusca rubra"]<-"Festuca rubra"
silwood21data$Species[silwood21data$Species=="Ranculus repens"]<-"Ranunculus repens"
silwood21data$Species[silwood21data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
silwood21data$Species[silwood21data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
silwood21data$Species[silwood21data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
silwood21data$Species[silwood21data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
silwood21data$Species[silwood21data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
silwood21data$Species[silwood21data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
silwood21data$Species[silwood21data$Species=="Circium arvense"]<-"Cirsium arvense"
silwood21data$Species[silwood21data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
silwood21data$Species[silwood21data$Species=="Poa trivalis"]<-"Poa trivialis"

#see list of species names
unique(silwood21data$Species)

#fix date column format
silwood21data$Date<-as.Date(silwood21data$Date)
#add year columns
silwood21data$Year <- as.numeric(format(silwood21data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(silwood21data, 'silwood21.xlsx')

