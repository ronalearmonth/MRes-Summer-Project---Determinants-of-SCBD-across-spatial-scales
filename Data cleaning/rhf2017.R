### RHF COVER 2017 ###

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
rhf17data<-read.csv("rhf_2017_cover.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(rhf17data$Species)

#manually remove dodgy entries
#remove notes column
rhf17data<-rhf17data%>%filter(Species!="moss"&Species!="rock"&!grepl("poo",Species)&
                                !grepl("soil",Species)&!grepl("log",Species)&
                                Species!="pointy"&Species!="stone"&Species!="unknown trifoliate"&
                                Species!="finger"&Species!="opposite heart"&
                                Species!="heat singed"&Species!="rough leaf"&
                                Species!="grass"&Species!="variegated"&Species!="variagated"&
                                Species!="yellow head"&Species!="new sticky"&
                                Species!="spines"&Species!="perennial"&Species!="heat shock"&
                                Species!="fungus"&Species!="rosette"&Species!="odd"&
                                Species!="dead sage bark"&Species!="dandylion-esque leaf"&Species!="tiny thing"&
                                Species!="dead sage root"&Species!="dead grass"&
                                Species!="emerged leaf"&Species!="tiny white"&
                                Species!="burnt annual"&Species!="sprout rosette"&
                                Species!="dead branch"&Species!="pine needles"&
                                Species!="Twigs / broken trunk"&Species!="twigs"&
                                Species!="raceme"&Species!="trifoliate"&
                                Species!="grass")%>%
  select(Date,Recorder,Site,Quadrant,Species,Cover)%>%
  mutate(Species = case_when(str_detect(Species, "cabbage") ~ "Brassicaceae",TRUE~Species))
#potentially Brassicaceae should be removed

rhf17data$Species[rhf17data$Species=="mustard"]<-"Brassica sp."
rhf17data$Species[rhf17data$Species=="asparagus"]<-"Asparagus sp."
rhf17data$Species[rhf17data$Species=="clover"]<-"Trifolium repens"


#remove text in brackets
rhf17data$Species<-gsub(r"{\s*\([^\)]+\)}","",rhf17data$Species)
#remove text after dash
rhf17data$Species<-gsub("-.*","",rhf17data$Species)


#rename bad spelling
rhf17data$Species[rhf17data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
rhf17data$Species[rhf17data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
rhf17data$Species[rhf17data$Species=="Vicia fava"]<-"Vicia faba"
rhf17data$Species[rhf17data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
rhf17data$Species[rhf17data$Species=="Succisa pratensid"]<-"Succisa pratensis"
rhf17data$Species[rhf17data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
rhf17data$Species[rhf17data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
rhf17data$Species[rhf17data$Species=="Trifolium campastre"]<-"Trifolium campestre"
rhf17data$Species[rhf17data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
rhf17data$Species[rhf17data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
rhf17data$Species[rhf17data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
rhf17data$Species[rhf17data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
rhf17data$Species[rhf17data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
rhf17data$Species[rhf17data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
rhf17data$Species[rhf17data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
rhf17data$Species[rhf17data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
rhf17data$Species[rhf17data$Species=="Rubus ideaus"]<-"Rubus idaeus"
rhf17data$Species[rhf17data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
rhf17data$Species[rhf17data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
rhf17data$Species[rhf17data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
rhf17data$Species[rhf17data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
rhf17data$Species[rhf17data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
rhf17data$Species[rhf17data$Species=="Circium undulatum"]<-"Cirsium undulatum"
rhf17data$Species[rhf17data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
rhf17data$Species[rhf17data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
rhf17data$Species[rhf17data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
rhf17data$Species[rhf17data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
rhf17data$Species[rhf17data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
rhf17data$Species[rhf17data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
rhf17data$Species[rhf17data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
rhf17data$Species[rhf17data$Species=="Aster engelmanii"]<-"Aster engelmannii"
rhf17data$Species[rhf17data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
rhf17data$Species[rhf17data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
rhf17data$Species[rhf17data$Species=="Lomatium greyi"]<-"Lomatium grayi"
rhf17data$Species[rhf17data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
rhf17data$Species[rhf17data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
rhf17data$Species[rhf17data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
rhf17data$Species[rhf17data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
rhf17data$Species[rhf17data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
rhf17data$Species[rhf17data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
rhf17data$Species[rhf17data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
rhf17data$Species[rhf17data$Species=="Holcus molis"]<-"Holcus mollis"
rhf17data$Species[rhf17data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
rhf17data$Species[rhf17data$Species=="Luzulla campestris"]<-"Luzula campestris"
rhf17data$Species[rhf17data$Species=="Fetusca rubra"]<-"Festuca rubra"
rhf17data$Species[rhf17data$Species=="Ranculus repens"]<-"Ranunculus repens"
rhf17data$Species[rhf17data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
rhf17data$Species[rhf17data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
rhf17data$Species[rhf17data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
rhf17data$Species[rhf17data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
rhf17data$Species[rhf17data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
rhf17data$Species[rhf17data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
rhf17data$Species[rhf17data$Species=="Circium arvense"]<-"Cirsium arvense"
rhf17data$Species[rhf17data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
rhf17data$Species[rhf17data$Species=="Poa trivalis"]<-"Poa trivialis"


#check list of species names
unique(rhf17data$Species)

#fix date column format
rhf17data$Date<-as.Date(rhf17data$Date)
#add year columns
rhf17data$Year <- as.numeric(format(rhf17data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(rhf17data, 'rhf17.xlsx')
