### EXTRACTING TRAIT VALUES FROM BIEN ###

rm(list=ls())

library(BIEN)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(openxlsx)

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data")

#-------------------------------------------------------------------------------

## LOAD AND MAKE SURE COLUMN NAMES MATCH ACROSS ALL SITES AND YEARS ##


#load all the data and make all columns match
boothby22 <- read_excel("boothby22.xlsx")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
boothby23<-read.csv("boothby23.csv")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
budworth21 <- read_excel("budworth21.xlsx")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
enez21 <- read_excel("enez21.xlsx")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
knepp22 <- read_excel("knepp22.xlsx")%>%mutate(Plot=paste(Block,Plot,sep=""))%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
knepp23 <- read_excel("knepp23.xlsx")%>%mutate(Plot=paste(Block,Plot,sep=""))%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
ordu21 <- read_excel("ordu21.xlsx")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
archbold21 <- read_excel("archbold21.xlsx") %>%mutate(Site="Archbold",Year="2021")%>%mutate_all(~gsub("Quad_", "", .))%>%
  select(Site,Site_naming_scheme,Quadrat,Species,Cover,Year)%>%
  rename(Plot=Site_naming_scheme, Quadrant=Quadrat)
reinischikogel21 <- read_excel("reinischkogel21.xlsx")%>%mutate(Quadrant=as.factor(match(Block, LETTERS)))%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
rhf17 <- read_excel("rhf17.xlsx")%>%rename(Plot=Site)%>%mutate(Site="rhf")%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
rhf18 <- read_excel("rhf18.xlsx")%>%rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
rhf19 <- read_excel("rhf19.xlsx")%>%rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
rhf20 <- read_excel("rhf20.xlsx")%>%rename(Plot=Plot_id)%>%mutate(Site="rhf")%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
silwood21 <- read_excel("silwood21.xlsx")%>%rename(Plot=Site)%>%mutate(Site="Silwood Park")%>%
  select(Site,Plot,Quadrant,Species,Cover,Year)
silwood22 <- read_excel("silwood22.xlsx")%>%select(Site,Plot,Quadrant,Species,Cover,Year)
silwood23<-read.csv("silwood23.csv")%>%select(Site,Plot,Quadrant,Species,Cover,Year)

#collapse four-level rhf into 3-level, remove letters
rhf<-rbind(rhf18,rhf19,rhf20)
rhf$Plot<-gsub("A","",rhf$Plot)
rhf$Plot<-gsub("B","",rhf$Plot)
rhf$Plot<-gsub("C","",rhf$Plot)
rhf$Plot<-gsub("D","",rhf$Plot)
rhf$Plot<-substr(rhf$Plot,1,3)

#create one big df
alldata<-rbind(boothby22,boothby23,budworth21,enez21,knepp22,knepp23,ordu21,archbold21,
               reinischikogel21,rhf17,rhf,silwood21,silwood22)

#separate into genus and species
speciesonly_df<-alldata%>%filter(!grepl(" sp.",Species)|Species=="Prunus spinosa")%>%
  filter(Species!="grass"&!grepl("ceae",Species)
         &Species!="Caloboletus calopus"&Species!="dark purple Brassica"&
           Species!="Nemophila"&Species!="Amanita rubescens"&!grepl("Cladonia",Species))

#rename bad spelling
speciesonly_df$Species[speciesonly_df$Species=="Rubus fructicosus"]<-"Rubus fruticosus"
speciesonly_df$Species[speciesonly_df$Species=="Rubus fruticosus agg."]<-"Rubus fruticosus"
speciesonly_df$Species[speciesonly_df$Species=="Chamaenerion augustifolium"]<-"Chamaenerion angustifolium"
speciesonly_df$Species[speciesonly_df$Species=="Geranium robertianium"]<-"Geranium robertianum"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichiana"]<-"Lachnocaulon beyrichianum"
speciesonly_df$Species[speciesonly_df$Species=="Vicia tetraspermae"]<-"Vicia tetrasperma"
speciesonly_df$Species[speciesonly_df$Species=="Vicia fava"]<-"Vicia faba"
speciesonly_df$Species[speciesonly_df$Species=="Fallopia convolvulvus"]<-"Fallopia convolvulus"
speciesonly_df$Species[speciesonly_df$Species=="Succisa pratensid"]<-"Succisa pratensis"
speciesonly_df$Species[speciesonly_df$Species=="Deschampsia caespitosa"]<-"Deschampsia cespitosa"
speciesonly_df$Species[speciesonly_df$Species=="Sorbus acuparia"]<-"Sorbus aucuparia"
speciesonly_df$Species[speciesonly_df$Species=="Trifolium campastre"]<-"Trifolium campestre"
speciesonly_df$Species[speciesonly_df$Species=="Lotus pendunculatus"]<-"Lotus pedunculatus"
speciesonly_df$Species[speciesonly_df$Species=="Circaea lutetiansa"]<-"Circaea lutetiana"
speciesonly_df$Species[speciesonly_df$Species=="Artemisia vulgari"]<-"Artemisia vulgaris"
speciesonly_df$Species[speciesonly_df$Species=="Rhynchosopra megalocarpa"]<-"Rhynchospora megalocarpa"
speciesonly_df$Species[speciesonly_df$Species=="Fuirena scirpoides"]<-"Fuirena scirpoidea"
speciesonly_df$Species[speciesonly_df$Species=="Lachnocaulon beyrichianum"]<-"Lachnocaulon beyrichiana"
speciesonly_df$Species[speciesonly_df$Species=="Campanula ranunculoides"]<-"Campanula rapunculoides"
speciesonly_df$Species[speciesonly_df$Species=="Rubus ideaus"]<-"Rubus idaeus"
speciesonly_df$Species[speciesonly_df$Species=="Lathyrus paviflorus"]<-" Lathyrus pauciflorus"
speciesonly_df$Species[speciesonly_df$Species=="Taraxacum officinalis"]<-"Taraxacum officinale"
speciesonly_df$Species[speciesonly_df$Species=="Mainthemum stellatum"]<-"Maianthemum stellatum"
speciesonly_df$Species[speciesonly_df$Species=="Physocarpous malvaceus"]<-"Physocarpus malvaceus"
speciesonly_df$Species[speciesonly_df$Species=="Mainthemum racemosum"]<-"Maianthemum racemosum"
speciesonly_df$Species[speciesonly_df$Species=="Circium undulatum"]<-"Cirsium undulatum"
speciesonly_df$Species[speciesonly_df$Species=="Paxistima myrsinitis"]<-"Paxistima myrsinites"
speciesonly_df$Species[speciesonly_df$Species=="Artemesia tridentata"]<-"Artemisia tridentata"
speciesonly_df$Species[speciesonly_df$Species=="Mitellla stauropetala"]<-"Mitella stauropetala"
speciesonly_df$Species[speciesonly_df$Species=="Comandra umbellatum"]<-"Comandra umbellata"
speciesonly_df$Species[speciesonly_df$Species=="Castillea linariifolia"]<-"Castilleja linariifolia"
speciesonly_df$Species[speciesonly_df$Species=="Cerocarpous ledifolius"]<-"Cercocarpus ledifolius"
speciesonly_df$Species[speciesonly_df$Species=="Rudebeckia occidentalis"]<-"Rudbeckia occidentalis"
speciesonly_df$Species[speciesonly_df$Species=="Aster engelmanii"]<-"Aster engelmannii"
speciesonly_df$Species[speciesonly_df$Species=="Koaleria macrantha"]<-"Koeleria macrantha"
speciesonly_df$Species[speciesonly_df$Species=="Delphinium nutalianum"]<-"Delphinium nuttallianum"
speciesonly_df$Species[speciesonly_df$Species=="Lomatium greyi"]<-"Lomatium grayi"
speciesonly_df$Species[speciesonly_df$Species=="Clatonia perfoliata"]<-"Claytonia perfoliata"
speciesonly_df$Species[speciesonly_df$Species=="Chrysothamnus vicidiflorus"]<-"Chrysothamnus viscidiflorus"
speciesonly_df$Species[speciesonly_df$Species=="Ipomopsis agregatta"]<-"Ipomopsis aggregata"
speciesonly_df$Species[speciesonly_df$Species=="Physocarpos malvaceus"]<-"Physocarpus malvaceus"
speciesonly_df$Species[speciesonly_df$Species=="Thallictrum fendlerii"]<-"Thalictrum fendleri"
speciesonly_df$Species[speciesonly_df$Species=="Amelenchier alnifolia"]<-"Amelanchier alnifolia"
speciesonly_df$Species[speciesonly_df$Species=="Arrhenatherium elatius"]<-"Arrhenatherum elatius"
speciesonly_df$Species[speciesonly_df$Species=="Holcus molis"]<-"Holcus mollis"
speciesonly_df$Species[speciesonly_df$Species=="Impatiens grandiflora"]<-"Impatiens glandulifera"
speciesonly_df$Species[speciesonly_df$Species=="Luzulla campestris"]<-"Luzula campestris"
speciesonly_df$Species[speciesonly_df$Species=="Fetusca rubra"]<-"Festuca rubra"
speciesonly_df$Species[speciesonly_df$Species=="Ranculus repens"]<-"Ranunculus repens"
speciesonly_df$Species[speciesonly_df$Species=="Jacobea vulgaris"]<-"Jacobaea vulgaris"
speciesonly_df$Species[speciesonly_df$Species=="Linium teniufolium"]<-"Linum tenuifolium"
speciesonly_df$Species[speciesonly_df$Species=="Tristenum flavescens"]<-"Trisetum flavescens"
speciesonly_df$Species[speciesonly_df$Species=="Angelica silvestris"]<-"Angelica sylvestris"
speciesonly_df$Species[speciesonly_df$Species=="Engeron canadensis"]<-"Erigeron canadensis"
speciesonly_df$Species[speciesonly_df$Species=="Andropogon brachystachys"]<-"Andropogon brachystachyus"

#save species df as excel file
write.xlsx(speciesonly_df,"invasivelist.xlsx")

#-------------------------------------------------------------------------------

## EXTRACT TRAIT DATA ##

#calculate three major traits for each species in list

traitlist<-c("seed mass","whole plant height","leaf area per leaf dry mass")
species_list<-unique(speciesonly_df$Species)

traitdf<-BIEN_trait_traitbyspecies(trait=traitlist,species=species_list)%>%
  select(scrubbed_species_binomial,trait_name,trait_value)%>%group_by(scrubbed_species_binomial,trait_name)
traitdf$trait_value<-as.numeric(traitdf$trait_value)

#try to remove duplicates
traitdf<-traitdf%>%group_by(scrubbed_species_binomial,trait_name)%>%
  summarise(meanvalue=mean(as.numeric(trait_value)))%>%
  pivot_wider(names_from=trait_name,values_from=meanvalue)

#match trait data back onto big dataset based on species

#create seedmass column
speciesonly_df$SLA<-traitdf$'leaf area per leaf dry mass'[
  match(speciesonly_df$Species,traitdf$scrubbed_species_binomial )]

#create height column
speciesonly_df$Height<-traitdf$`whole plant height`[
  match(speciesonly_df$Species,traitdf$scrubbed_species_binomial )]

#create SLA column
speciesonly_df$Seed_mass<-traitdf$`seed mass`[
  match(speciesonly_df$Species,traitdf$scrubbed_species_binomial )]

traitdata<-speciesonly_df


#-------------------------------------------------------------------------------

## WRITE TO EXCEL AND ASSIGN NATIVE OR INTRODUCED STATUS ##

#save to excel, then manually fill in gaps
write.xlsx(teststatus,"teststatus.xlsx")

#-------------------------------------------------------------------------------

## ASSIGN NATIVE VS INTRODUCED STATUS ##

#open xlsx sheet with manually assigned invasive status from Kew POWO
nativestatus<-read.xlsx("test2.xlsx")

#match invasive status of already catalogued species onto df
traitdata<-left_join(traitdata,nativestatus,relationship = "many-to-many")

#-------------------------------------------------------------------------------

## SAVE TRAIT DATA ##

write.csv(traitdata,"traitdata.csv")

#end