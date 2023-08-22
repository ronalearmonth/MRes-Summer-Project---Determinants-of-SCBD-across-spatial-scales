### BUDWORTH 2021 ###

rm(list=ls())

#load required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)

#set wd
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-rawdata")

#import datasheet
budworth21data<-read_excel("Brearley_EcoFrac_2021.xlsx")

## DATA CLEANING ##

#see list of 'species' names
unique(budworth21data$Species)

#manually remove dodgy entries
#remove notes column
budworth21data<-budworth21data%>%filter(Species!="Bare"&Species!="Sp. A"&
                                Species!="NA"&Species!="Moss Sp. A"&
                                Species!="Sp. C"&Species!="Tree Sp. B"&
                                Species!="Moss Sp. B (feathery)"&Species!="Moss Sp. C (spiky)"&
                                  Species!="Moss sp. D"&
                                Species!="Bare/Litter")%>%
  select(Date,Recorder,Site,Plot,Quadrant,Species,Cover)

#fix spelling
budworth21data$Species[budworth21data$Species=="Querus sp."]<-"Quercus sp."
budworth21data$Species[budworth21data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
budworth21data$Species[budworth21data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
budworth21data$Species[budworth21data$Species=="Vicia fava"]<-"Vicia faba"
budworth21data$Species[budworth21data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
budworth21data$Species[budworth21data$Species=="Succisa pratensid"]<-"Succisa pratensis"
budworth21data$Species[budworth21data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
budworth21data$Species[budworth21data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
budworth21data$Species[budworth21data$Species=="Trifolium campastre"]<-"Trifolium campestre"
budworth21data$Species[budworth21data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
budworth21data$Species[budworth21data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
budworth21data$Species[budworth21data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
budworth21data$Species[budworth21data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
budworth21data$Species[budworth21data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
budworth21data$Species[budworth21data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
budworth21data$Species[budworth21data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
budworth21data$Species[budworth21data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
budworth21data$Species[budworth21data$Species=="Rubus ideaus"]<-"Rubus idaeus"
budworth21data$Species[budworth21data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
budworth21data$Species[budworth21data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
budworth21data$Species[budworth21data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
budworth21data$Species[budworth21data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
budworth21data$Species[budworth21data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
budworth21data$Species[budworth21data$Species=="Circium undulatum"]<-"Cirsium undulatum"
budworth21data$Species[budworth21data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
budworth21data$Species[budworth21data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
budworth21data$Species[budworth21data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
budworth21data$Species[budworth21data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
budworth21data$Species[budworth21data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
budworth21data$Species[budworth21data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
budworth21data$Species[budworth21data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
budworth21data$Species[budworth21data$Species=="Aster engelmanii"]<-"Aster engelmannii"
budworth21data$Species[budworth21data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
budworth21data$Species[budworth21data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
budworth21data$Species[budworth21data$Species=="Lomatium greyi"]<-"Lomatium grayi"
budworth21data$Species[budworth21data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
budworth21data$Species[budworth21data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
budworth21data$Species[budworth21data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
budworth21data$Species[budworth21data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
budworth21data$Species[budworth21data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
budworth21data$Species[budworth21data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
budworth21data$Species[budworth21data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
budworth21data$Species[budworth21data$Species=="Holcus molis"]<-"Holcus mollis"
budworth21data$Species[budworth21data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
budworth21data$Species[budworth21data$Species=="Luzulla campestris"]<-"Luzula campestris"
budworth21data$Species[budworth21data$Species=="Fetusca rubra"]<-"Festuca rubra"
budworth21data$Species[budworth21data$Species=="Ranculus repens"]<-"Ranunculus repens"
budworth21data$Species[budworth21data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
budworth21data$Species[budworth21data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
budworth21data$Species[budworth21data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
budworth21data$Species[budworth21data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
budworth21data$Species[budworth21data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
budworth21data$Species[budworth21data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
budworth21data$Species[budworth21data$Species=="Circium arvense"]<-"Cirsium arvense"
budworth21data$Species[budworth21data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
budworth21data$Species[budworth21data$Species=="Poa trivalis"]<-"Poa trivialis"


#check list of species names
unique(budworth21data$Species)

#fix date column format
budworth21data$Date<-as.Date(budworth21data$Date)
#add year columns
budworth21data$Year <- as.numeric(format(budworth21data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(budworth21data, 'budworth21.xlsx')
