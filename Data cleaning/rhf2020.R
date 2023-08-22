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
rhf20data<-read.csv("rhf_2020_cover.csv",header=T)

## DATA CLEANING ##

#see list of 'species' names
unique(rhf20data$Species)

#manually remove dodgy entries
#remove notes column
rhf20data<-rhf20data%>%filter(Species!="moss"&Species!="rocks"&!grepl("poo",Species)&
                                !grepl("soil",Species)&Species!="animal hair"&
                                        Species!="organic matter"&Species!="broad-leafed grass"&
                                Species!="dried yellow onion leaf?"&Species!="fuzzy little rosette"&
                                Species!="whorled tiny leaved herb"&Species!="teeny tiny plant"&
                                Species!="white umbel feather leaved plant"&
                                !grepl("log",Species)&Species!="long leaved shiny droopy grass"&
                                Species!="thin leaved grass"&Species!="basal rosette of leaves"&
                                Species!="gray lichen"&Species!="woody sprout"&Species!="Elymus or Bromus sp.?")%>%
  select(Date,Recorder_1,Plot_id,Quadrant,Species,Cover)%>%
#rename chickweed
  mutate(Species = case_when(str_detect(Species, "chickweed") ~ "Stellaria sp.",TRUE~Species))

#rename asteraceae and asteraceae-opposite leaves to "Aster_sp."
rhf20data$Species[rhf20data$Species=="asteraceae"|rhf20data$Species=="asteraceae-opposite leaves"]<-"Asteraceae"
rhf20data$Species[rhf20data$Species=="rocky mountain daisy aster"]<-"Erigeron subtrinervis"
rhf20data$Species[rhf20data$Species=="hear leafed violet"]<-"Viola sp."
rhf20data$Species[rhf20data$Species=="rice grass"]<-"Oryzopsis hymenoides"

#potentially Asteraceae should be removed

#remove text in brackets
rhf20data$Species<-gsub(r"{\s*\([^\)]+\)}","",rhf20data$Species)
#remove text after dash
rhf20data$Species<-gsub("-.*","",rhf20data$Species)

#rename bad spelling
rhf20data$Species[rhf20data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
rhf20data$Species[rhf20data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
rhf20data$Species[rhf20data$Species=="Vicia fava"]<-"Vicia faba"
rhf20data$Species[rhf20data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
rhf20data$Species[rhf20data$Species=="Succisa pratensid"]<-"Succisa pratensis"
rhf20data$Species[rhf20data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
rhf20data$Species[rhf20data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
rhf20data$Species[rhf20data$Species=="Trifolium campastre"]<-"Trifolium campestre"
rhf20data$Species[rhf20data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
rhf20data$Species[rhf20data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
rhf20data$Species[rhf20data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
rhf20data$Species[rhf20data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
rhf20data$Species[rhf20data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
rhf20data$Species[rhf20data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
rhf20data$Species[rhf20data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
rhf20data$Species[rhf20data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
rhf20data$Species[rhf20data$Species=="Rubus ideaus"]<-"Rubus idaeus"
rhf20data$Species[rhf20data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
rhf20data$Species[rhf20data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
rhf20data$Species[rhf20data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
rhf20data$Species[rhf20data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
rhf20data$Species[rhf20data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
rhf20data$Species[rhf20data$Species=="Circium undulatum"]<-"Cirsium undulatum"
rhf20data$Species[rhf20data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
rhf20data$Species[rhf20data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
rhf20data$Species[rhf20data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
rhf20data$Species[rhf20data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
rhf20data$Species[rhf20data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
rhf20data$Species[rhf20data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
rhf20data$Species[rhf20data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
rhf20data$Species[rhf20data$Species=="Aster engelmanii"]<-"Aster engelmannii"
rhf20data$Species[rhf20data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
rhf20data$Species[rhf20data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
rhf20data$Species[rhf20data$Species=="Lomatium greyi"]<-"Lomatium grayi"
rhf20data$Species[rhf20data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
rhf20data$Species[rhf20data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
rhf20data$Species[rhf20data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
rhf20data$Species[rhf20data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
rhf20data$Species[rhf20data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
rhf20data$Species[rhf20data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
rhf20data$Species[rhf20data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
rhf20data$Species[rhf20data$Species=="Holcus molis"]<-"Holcus mollis"
rhf20data$Species[rhf20data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
rhf20data$Species[rhf20data$Species=="Luzulla campestris"]<-"Luzula campestris"
rhf20data$Species[rhf20data$Species=="Fetusca rubra"]<-"Festuca rubra"
rhf20data$Species[rhf20data$Species=="Ranculus repens"]<-"Ranunculus repens"
rhf20data$Species[rhf20data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
rhf20data$Species[rhf20data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
rhf20data$Species[rhf20data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
rhf20data$Species[rhf20data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
rhf20data$Species[rhf20data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
rhf20data$Species[rhf20data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
rhf20data$Species[rhf20data$Species=="Circium arvense"]<-"Cirsium arvense"
rhf20data$Species[rhf20data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
rhf20data$Species[rhf20data$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(rhf20data$Species)

#fix date column format
rhf20data$Date<-as.Date(rhf20data$Date)
#add year columns
rhf20data$Year <- as.numeric(format(rhf20data$Date,'%Y'))


#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(rhf20data, 'rhf20.xlsx')


