### REINISCHKOGEL 2021 ###

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
reinischkogel21data<-read.csv("fractalnet_AClark_2021.csv",header=T)

## sort out plot naming scheme
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="1"]<-"111"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="2"]<-"113"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="3"]<-"131"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="4"]<-"133"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="5"]<-"132"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="6"]<-"123"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="7"]<-"122"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="8"]<-"121"
reinischkogel21data$Plot2[reinischkogel21data$Block=="A"&reinischkogel21data$Plot=="9"]<-"112"

reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="1"]<-"211"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="2"]<-"213"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="3"]<-"231"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="4"]<-"233"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="5"]<-"232"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="6"]<-"223"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="7"]<-"222"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="8"]<-"221"
reinischkogel21data$Plot2[reinischkogel21data$Block=="B"&reinischkogel21data$Plot=="9"]<-"212"

reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="1"]<-"311"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="2"]<-"313"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="3"]<-"331"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="4"]<-"333"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="5"]<-"332"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="6"]<-"323"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="7"]<-"322"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="8"]<-"321"
reinischkogel21data$Plot2[reinischkogel21data$Block=="C"&reinischkogel21data$Plot=="9"]<-"312"

reinischkogel21data$Plot<-reinischkogel21data$Plot2


## DATA CLEANING ##

#see list of 'species' names
unique(reinischkogel21data$Species)

#manually remove dodgy entries
#remove notes column
reinischkogel21data<-reinischkogel21data%>%filter(Species!="Moss"&Species!="Dead wood"&
                               Species!="Litter"&Species!="Bare ground")%>%
  select(Date,Recorder,Site,Block, Plot,Quadrant,Species,Cover)


#rename bad spelling
reinischkogel21data$Species[reinischkogel21data$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
reinischkogel21data$Species[reinischkogel21data$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
reinischkogel21data$Species[reinischkogel21data$Species=="Vicia fava"]<-"Vicia faba"
reinischkogel21data$Species[reinischkogel21data$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
reinischkogel21data$Species[reinischkogel21data$Species=="Succisa pratensid"]<-"Succisa pratensis"
reinischkogel21data$Species[reinischkogel21data$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
reinischkogel21data$Species[reinischkogel21data$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
reinischkogel21data$Species[reinischkogel21data$Species=="Trifolium campastre"]<-"Trifolium campestre"
reinischkogel21data$Species[reinischkogel21data$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
reinischkogel21data$Species[reinischkogel21data$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
reinischkogel21data$Species[reinischkogel21data$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
reinischkogel21data$Species[reinischkogel21data$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
reinischkogel21data$Species[reinischkogel21data$Species=="Fuirena scirpoidea"]<-"Fuirena scirpoides"
reinischkogel21data$Species[reinischkogel21data$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
reinischkogel21data$Species[reinischkogel21data$Species=="Andropogon brachystachyus"]<-"Andropogon brachystachys"
reinischkogel21data$Species[reinischkogel21data$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
reinischkogel21data$Species[reinischkogel21data$Species=="Rubus ideaus"]<-"Rubus idaeus"
reinischkogel21data$Species[reinischkogel21data$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
reinischkogel21data$Species[reinischkogel21data$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
reinischkogel21data$Species[reinischkogel21data$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
reinischkogel21data$Species[reinischkogel21data$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
reinischkogel21data$Species[reinischkogel21data$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
reinischkogel21data$Species[reinischkogel21data$Species=="Circium undulatum"]<-"Cirsium undulatum"
reinischkogel21data$Species[reinischkogel21data$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
reinischkogel21data$Species[reinischkogel21data$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
reinischkogel21data$Species[reinischkogel21data$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
reinischkogel21data$Species[reinischkogel21data$Species=="Comandra umbellatum"]<-"Comandra umbellata"
reinischkogel21data$Species[reinischkogel21data$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
reinischkogel21data$Species[reinischkogel21data$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
reinischkogel21data$Species[reinischkogel21data$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
reinischkogel21data$Species[reinischkogel21data$Species=="Aster engelmanii"]<-"Aster engelmannii"
reinischkogel21data$Species[reinischkogel21data$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
reinischkogel21data$Species[reinischkogel21data$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
reinischkogel21data$Species[reinischkogel21data$Species=="Lomatium greyi"]<-"Lomatium grayi"
reinischkogel21data$Species[reinischkogel21data$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
reinischkogel21data$Species[reinischkogel21data$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
reinischkogel21data$Species[reinischkogel21data$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
reinischkogel21data$Species[reinischkogel21data$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
reinischkogel21data$Species[reinischkogel21data$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
reinischkogel21data$Species[reinischkogel21data$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
reinischkogel21data$Species[reinischkogel21data$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
reinischkogel21data$Species[reinischkogel21data$Species=="Holcus molis"]<-"Holcus mollis"
reinischkogel21data$Species[reinischkogel21data$Species=="Impatients grandiflora"]<-"Impatiens grandiflora"
reinischkogel21data$Species[reinischkogel21data$Species=="Luzulla campestris"]<-"Luzula campestris"
reinischkogel21data$Species[reinischkogel21data$Species=="Fetusca rubra"]<-"Festuca rubra"
reinischkogel21data$Species[reinischkogel21data$Species=="Ranculus repens"]<-"Ranunculus repens"
reinischkogel21data$Species[reinischkogel21data$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
reinischkogel21data$Species[reinischkogel21data$Species=="Linium teniufolium"]<-"Linum tenuifolium"
reinischkogel21data$Species[reinischkogel21data$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
reinischkogel21data$Species[reinischkogel21data$Species=="Angelica silvestris"]<-"Angelica sylvestris"
reinischkogel21data$Species[reinischkogel21data$Species=="Engeron canadensis"]<-"Erigeron canadensis"
reinischkogel21data$Species[reinischkogel21data$Species=="Delphinium occidentalis"]<-"Delphinium occidentale"
reinischkogel21data$Species[reinischkogel21data$Species=="Circium arvense"]<-"Cirsium arvense"
reinischkogel21data$Species[reinischkogel21data$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
reinischkogel21data$Species[reinischkogel21data$Species=="Poa trivalis"]<-"Poa trivialis"

#check list of species names
unique(reinischkogel21data$Species)

#fix date column format
reinischkogel21data$Date<-as.Date(reinischkogel21data$Date)
#add year columns
reinischkogel21data$Year <- as.numeric(format(reinischkogel21data$Date,'%Y'))

#save as excel file
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data") #set new wd
write.xlsx(reinischkogel21data, 'reinischkogel21.xlsx')
