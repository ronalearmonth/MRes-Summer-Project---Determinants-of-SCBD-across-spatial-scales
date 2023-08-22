### SPECIES CONTRIBUTIONS TO BETA DIVERSITY ###

#clear memory
rm(list=ls())

# Load packages
library(adespatial)
library(dplyr)
library(readxl)
library(stringr)
library(purrr)
library(tidyr)
library(openxlsx)

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/ecofrac-cleaned-data")

#-------------------------------------------------------------------------------

## LOAD AND TIDY TRAIT DATA ##

traitdata<-read.csv("traitdata.csv",header=TRUE)%>%select(!X)

#remove plots named A-E, 0 - silwood hexagons
traitdata<-traitdata%>%filter(Plot!="A"&Plot!="B"&
                                Plot!="C"&Plot!="D"&
                                Plot!="E"&Plot!="F"&Plot!=""&
                                Plot!="C0"&
                                Plot!="0"&!is.na(Plot))

#remove * from plot names
traitdata$Plot<-str_replace(traitdata$Plot, '\\*', '')

#check plot names are sane
unique(traitdata$Plot)

#create major and minor triad identifiers - include block for boothby/knepp 

traitdata$Majortriad<-str_sub(traitdata$Plot,1,-3)
traitdata$Minortriad<-str_sub(traitdata$Plot,-2,-2)


#-------------------------------------------------------------------------------

## MAJOR TRIADS WITHIN EACH SITE ##

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdmajor<-function(x){
  temp <- sample2matrix(data.frame(x$Majortriad, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_major=temp2,row.names=NULL)
}

#create data list
traitdata_list_major=split(traitdata,list(traitdata$Site,traitdata$Year),drop=TRUE)

#drop silwood park.2021 - no variation in beta.div
traitdata_list_major$`Silwood Park.2021`=NULL

#apply function to list 
majorbetalist<-map(traitdata_list_major,calc_scbdmajor)

#get site/year id for each 
majorbetalist<-map2(majorbetalist,names(majorbetalist),~mutate(.x,ID=.y))

#make big df
majorbetadf<-do.call("rbind",majorbetalist)
ID<-as.data.frame(majorbetadf[c('Site','Year')]<-
  str_split_fixed(majorbetadf$ID,"[.]",2))

#remove ID column
majorbetadf<-majorbetadf%>%select(Species,Beta_contrib_major,Site,Year)

#make year format match
majorbetadf$Year<-as.integer(majorbetadf$Year)

#import trait data then match by site and species
traitbetadata<-left_join(traitdata,majorbetadf)


#-------------------------------------------------------------------------------

### MINOR TRIADS WITHIN EACH MAJOR TRIAD ###

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdminor<-function(x){
  temp <- sample2matrix(data.frame(x$Minortriad, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_minor=temp2,row.names=NULL)
}

#create data list
traitdata_list_minor=split(traitdata,list(traitdata$Site,traitdata$Year,traitdata$Majortriad),drop=TRUE)

#remove df with no beta.div variation
  #e.g. in knepp 2022 only one minor triad per block sampled
traitdata_list_minor$Budworth.2021.1=NULL
traitdata_list_minor$`Enez forest.2021.1`=NULL
traitdata_list_minor$`Enez forest.2021.3`=NULL
traitdata_list_minor$Ordu_urban.2021.1=NULL
traitdata_list_minor$`Silwood Park.2021.2`=NULL
traitdata_list_minor$Budworth.2021.3=NULL
traitdata_list_minor$Ordu_urban.2021.3=NULL
traitdata_list_minor$Knepp.2022.M2=NULL
traitdata_list_minor$Knepp.2022.M3=NULL
traitdata_list_minor$Knepp.2022.N2=NULL
traitdata_list_minor$Knepp.2022.N3=NULL
traitdata_list_minor$Knepp.2022.SX2=NULL
traitdata_list_minor$Knepp.2022.SX3=NULL
traitdata_list_minor$Knepp.2022.SY2=NULL
traitdata_list_minor$Knepp.2022.SY3=NULL
traitdata_list_minor$Knepp.2022.SZ2=NULL
traitdata_list_minor$Knepp.2022.SZ3=NULL
traitdata_list_minor$Boothby.2022.S3=NULL
traitdata_list_minor$Boothby.2022.C2=NULL


#apply function to list 
minorbetalist<-map(traitdata_list_minor,calc_scbdminor)

#give each row a site/year/majortriad id
minorbetalist<-map2(minorbetalist,names(minorbetalist),~mutate(.x,ID=.y))

#make big df
minorbetadf<-do.call("rbind",minorbetalist)

#split id into columns
ID<-minorbetadf[c('Site','Year','Majortriad')]<-
  str_split_fixed(minorbetadf$ID,"[.]",3)

#remove ID column
minorbetadf<-minorbetadf%>%select(Species,Beta_contrib_minor,Site,Year,Majortriad)

#make year format match
minorbetadf$Year<-as.integer(minorbetadf$Year)

#import trait data then match by site and species
traitbetadata2<-left_join(traitbetadata,minorbetadf)


#-------------------------------------------------------------------------------

### PLOT LEVEL WITHIN EACH MINOR TRIAD ###

#create function that calculates SCBD in one step to run on subsetted data
calc_scbdplot<-function(x){
  temp <- sample2matrix(data.frame(x$Plot, as.numeric(x$Cover), x$Species))
  # Contributions to beta-diversity
  temp2<-beta.div(temp)$SCBD
  #convert named numeric vector to df
  data.frame(Species=names(temp2),
             Beta_contrib_plot=temp2,row.names=NULL)
}

#create data list of each minor within each major
traitdata_list_plot=split(traitdata,list(traitdata$Site,traitdata$Year,traitdata$Majortriad,traitdata$Minortriad),drop=TRUE)


#remove minor triads which do not contain at least two plots
#because of boothby monocultures, need to include a -must have 2 or more species- condition
newlist <- list()

for (i in seq_along(traitdata_list_plot)) {
  df_i <- traitdata_list_plot[[i]]
  if (length(unique(df_i$Plot)) >= 2 &&
      length(unique(df_i$Species)) >= 2) {
    df_name <- names(traitdata_list_plot)[i]
    newlist[[df_name]] <- df_i
  }
}

# Remove null dataframes
newlist <- newlist[!sapply(newlist, is.null)]

#calculate scbd for all groups with beta.div variance
plotbetalist<-map(newlist,calc_scbdplot)

#give each row a site/year/plot id
plotbetalist<-map2(plotbetalist,names(plotbetalist),~mutate(.x,ID=.y))

#make big df
plotbetadf<-do.call("rbind",plotbetalist)

#split id into columns
ID<-plotbetadf[c('Site','Year','Majortriad','Minortriad')]<-
  str_split_fixed(plotbetadf$ID,"[.]",4)

#remove ID column
plotbetadf<-plotbetadf%>%select(Species,Beta_contrib_plot,Site,Year,Majortriad,Minortriad)

#make year format match
plotbetadf$Year<-as.integer(plotbetadf$Year)

#import trait data then match by site, year, majortriad, minortriad and species
traitbetadata3<-left_join(traitbetadata2,plotbetadf)


#save to excel 
write.csv(traitbetadata3,"plantdata.csv")

#end