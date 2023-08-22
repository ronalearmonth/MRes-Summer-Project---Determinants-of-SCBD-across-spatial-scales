### RHF COVER 2020 ###

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
rhf18data<-read.csv("rhf_2018_cover.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(rhf18data$Species)

#manually remove dodgy entries
#remove notes column
rhf18data<-rhf18data%>%filter(Species!="moss"&Species!="rock"&!grepl("poo",Species)&
                                !grepl("soil",Species)&!grepl("log",Species)&
                                Species!="browsed yellow"&
                                Species!="organic matter"&Species!="dead grass"&
                                Species!="tiny plant"&Species!="delicate vine"&
                                Species!="sticks"&Species!="burrow"&
                                Species!="lichen"&Species!="v senesced plant wth stalks munched off"&
                                Species!="basal_serrated leaf rossette_purple stem"&Species!="grass"&
                                Species!="gray lichen"&Species!="woody sprout"&Species!="Elymus or Bromus sp.?")%>%
  select(Date,Recorder_1,Plot_id,Quadrant,Species,Cover)%>%
  mutate(Species = case_when(str_detect(Species, "asteraceae") ~ "Asteraceae sp.",TRUE~Species))
#potentially Asteraceae should be removed

#remove text in brackets
rhf18data$Species<-gsub(r"{\s*\([^\)]+\)}","",rhf18data$Species)
#remove text after dash
rhf18data$Species<-gsub("-.*","",rhf18data$Species)

#rename bad spelling
rhf18data$Species[rhf18data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
rhf18data$Species[rhf18data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
rhf18data$Species[rhf18data$Species=="Vicia fava"]<-"Vicia faba"
rhf18data$Species[rhf18data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
rhf18data$Species[rhf18data$Species=="Succisa pratensid"]<-"Succisa pratensis"
rhf18data$Species[rhf18data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
rhf18data$Species[rhf18data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
rhf18data$Species[rhf18data$Species=="Trifolium campastre"]<-"Trifolium campestre"
rhf18data$Species[rhf18data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
rhf18data$Species[rhf18data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
rhf18data$Species[rhf18data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
rhf18data$Species[rhf18data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
rhf18data$Species[rhf18data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
rhf18data$Species[rhf18data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
rhf18data$Species[rhf18data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
rhf18data$Species[rhf18data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
rhf18data$Species[rhf18data$Species=="Rubus ideaus"]<-"Rubus idaeus"
rhf18data$Species[rhf18data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
rhf18data$Species[rhf18data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
rhf18data$Species[rhf18data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
rhf18data$Species[rhf18data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
rhf18data$Species[rhf18data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
rhf18data$Species[rhf18data$Species=="Circium undulatum"]<-"Cirsium undulatum"
rhf18data$Species[rhf18data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
rhf18data$Species[rhf18data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
rhf18data$Species[rhf18data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
rhf18data$Species[rhf18data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
rhf18data$Species[rhf18data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
rhf18data$Species[rhf18data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
rhf18data$Species[rhf18data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
rhf18data$Species[rhf18data$Species=="Aster engelmanii"]<-"Aster engelmannii"
rhf18data$Species[rhf18data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
rhf18data$Species[rhf18data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
rhf18data$Species[rhf18data$Species=="Lomatium greyi"]<-"Lomatium grayi"
rhf18data$Species[rhf18data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
rhf18data$Species[rhf18data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
rhf18data$Species[rhf18data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
rhf18data$Species[rhf18data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
rhf18data$Species[rhf18data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
rhf18data$Species[rhf18data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
rhf18data$Species[rhf18data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
rhf18data$Species[rhf18data$Species=="Holcus molis"]<-"Holcus mollis"
rhf18data$Species[rhf18data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
rhf18data$Species[rhf18data$Species=="Luzulla campestris"]<-"Luzula campestris"
rhf18data$Species[rhf18data$Species=="Fetusca rubra"]<-"Festuca rubra"
rhf18data$Species[rhf18data$Species=="Ranculus repens"]<-"Ranunculus repens"
rhf18data$Species[rhf18data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
rhf18data$Species[rhf18data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
rhf18data$Species[rhf18data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
rhf18data$Species[rhf18data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
rhf18data$Species[rhf18data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
rhf18data$Species[rhf18data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
rhf18data$Species[rhf18data$Species=="Circium arvense"]<-"Cirsium arvense"
rhf18data$Species[rhf18data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
rhf18data$Species[rhf18data$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(rhf18data$Species)

#fix date column format
rhf18data$Date<-as.Date(rhf18data$Date)
#add year columns
rhf18data$Year <- as.numeric(format(rhf18data$Date,'%Y'))


#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(rhf18data, 'rhf18.xlsx')
