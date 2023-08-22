### ALL MODELS AND FIGURES ###

#-------------------------------------------------------------------------------
#set up

rm(list=ls())

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network")

library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(betareg)
library(viridis)
library(beepr)
library(MuMIn)
library(dotwhisker)

#-------------------------------------------------------------------------------

## FRACTALS - MAJOR ##

plantdata<-read.csv("newmetriccompletedata.csv")%>%select(!X)

#create a binary native/introduced version
plantdata$Introduced_status[plantdata$Introduced_status=="Invasive"]<-"Introduced"

#z-scale continuous variables
plantdata$Phylo_dist_all<-as.numeric(scale(plantdata$ses.speciesmpd))
plantdata$SLA<-as.numeric(scale(plantdata$SLA))
plantdata$Height<-as.numeric(scale(plantdata$Height))
plantdata$Seed_mass<-as.numeric(scale(plantdata$Seed_mass))

#relevel introduction status 
plantdata$Introduced_status<-as.factor(plantdata$Introduced_status)
plantdata <- within(plantdata, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#try a maximal model
#remove na values
plantmajor<-plantdata%>%na.omit(Beta_contrib_major)

#build model
majormod<-betareg(Beta_contrib_major~
                    SLA+Seed_mass+
                    Height+ses.speciesmpd+
                    Introduced_status+
                    SLA*Introduced_status+
                    Seed_mass*Introduced_status+
                    Height*Introduced_status+
                    ses.speciesmpd*Introduced_status,
                  na.action=na.pass,data=plantmajor)

#dredge model subsets
majormodels<-model.avg(dredge(majormod))
majormod2<-dredge(majormod)
beep(3)


majormodsum<-summary(model.avg(majormod2,subset=delta<4,fit=TRUE))

#get AIC importance values
majormodsw<-sw(model.avg(majormod2,subset=delta<4,fit=TRUE))


#-------------------------------------------------------------------------------

## FRACTALS MINOR ##

#try a maximal model
plantminor<-plantdata%>%na.omit(Beta_contrib_minor)
#currently, doesnt work unless small constant is added to minor SCBD
plantminor$testbetaminor<-plantminor$Beta_contrib_minor+0.00000000000000000001

#build model
minormod<-betareg(testbetaminor~
                    SLA+Seed_mass+
                    Height+ses.speciesmpd+
                    Introduced_status+
                    SLA*Introduced_status+
                    Seed_mass*Introduced_status+
                    Height*Introduced_status+
                    ses.speciesmpd*Introduced_status,
                  na.action=na.pass,data=plantminor)

#dredge model subsets
minormodels<-model.avg(dredge(minormod))
minormod2<-dredge(minormod)
beep(3)

minormodsum<-summary(model.avg(minormod2,subset=delta<4,fit=TRUE))

#get AIC importance values
minormodsw<-(model.avg(minormod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------

## FRACTALS PLOT ##

plantplot<-plantdata%>%na.omit(Beta_contrib_plot)

#currently, doesnt work unless small constant is added to minor SCBD
plantplot$testbetaplot<-plantplot$Beta_contrib_plot+0.00000000000000000001

#relevel introduction status 
plantplot <- within(plantplot, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
plotmod<-betareg(testbetaplot~
                   SLA+Seed_mass+
                   Height+ses.speciesmpd+
                   Introduced_status+
                   SLA*Introduced_status+
                   Seed_mass*Introduced_status+
                   Height*Introduced_status+
                   ses.speciesmpd*Introduced_status,
                 na.action=na.pass,data=plantplot)

#dredge model subsets
plotmodels<-model.avg(dredge(plotmod))
plotmod2<-dredge(plotmod)
beep(3)

#plotmodsum<-summary(model.avg(plotmod2,subset=delta<4,fit=TRUE))

#get AIC importance values
#plotmodsw<-sw(model.avg(plotmod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------

## SITE LEVEL ##

setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/UK_GBIF/Finaldata2")

#site data
sitedata<-read.csv("newmetric.csv")%>%select(!X)

# ASSIGN NATIVE VS INTRODUCED STATUS #
#steal from UK data
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/UK_GBIF/Finaldata")

nativestatus<-read.csv("ukfinaldata.csv")%>%group_by(species,introduced_status)%>%
  summarise(count=n())%>%
  select(species,introduced_status)

#match invasive status of already catalogued species onto df
sitedata<-left_join(sitedata,nativestatus,relationship = "many-to-many")

#create a binary native/introduced version
sitedata$Introduced_status<-sitedata$introduced_status
sitedata$Introduced_status[sitedata$Introduced_status=="Invasive"]<-"Introduced"

#remove unknown introducedstatus
sitedata<-sitedata%>%na.omit(Introduced_status)

#z-scale continuous variables
sitedata$phylo_dist<-as.numeric(scale(sitedata$phylo_dist))
sitedata$ses.speciesmpd<-as.numeric(scale(sitedata$ses.speciesmpd))
sitedata$SLA<-as.numeric(scale(sitedata$SLA))
sitedata$Height<-as.numeric(scale(sitedata$Height))
sitedata$Seed_mass<-as.numeric(scale(sitedata$Seed_mass))

#relevel introduction status 
sitedata$Introduced_status<-as.factor(sitedata$Introduced_status)
sitedata <- within(sitedata, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
sitemod<-betareg(beta_contrib~
                   SLA+Seed_mass+
                   Height+ses.speciesmpd+
                   Introduced_status+
                   SLA*Introduced_status+
                   Seed_mass*Introduced_status+
                   Height*Introduced_status+
                   ses.speciesmpd*Introduced_status,
                 na.action=na.pass,data=sitedata)

#dredge model subsets
sitemodels<-model.avg(dredge(sitemod))
sitemod2<-dredge(sitemod)
beep(3)

sitemodsum<-summary(model.avg(sitemod2,subset=delta<4,fit=TRUE))

#get AIC importance values
sitemodsw<-sw(model.avg(sitemod2,subset=delta<4,fit=TRUE))

#-------------------------------------------------------------------------------

## UK DATA ##
setwd("C:/Users/rona/OneDrive - Imperial College London/Fractal network/UK_GBIF/Finaldata")

ukdata<-read.csv("ukfinaldata.csv")

#create a binary native/introduced version
ukdata$Introduced_status<-ukdata$introduced_status
ukdata$Introduced_status[ukdata$Introduced_status=="Invasive"]<-"Introduced"
ukdata$Introduced_status[ukdata$Introduced_status=="introduced"]<-"Introduced"

#remove unknown introducedstatus
ukdata<-ukdata%>%na.omit(Introduced_status)
ukdata<-ukdata%>%na.omit(beta_contrib)

#z-scale continuous variables
ukdata$ses.speciesmpd<-as.numeric(scale(ukdata$ses.speciesmpd))
ukdata$SLA<-as.numeric(scale(ukdata$SLA))
ukdata$height<-as.numeric(scale(ukdata$height))
ukdata$Seed_mass<-as.numeric(scale(ukdata$Seed_mass))

#relevel introduction status 
ukdata$Introduced_status<-as.factor(ukdata$Introduced_status)
ukdata <- within(ukdata, Introduced_status <- relevel(Introduced_status, ref = "Native"))

#build model
ukmod<-betareg(beta_contrib~
                 SLA+Seed_mass+
                 height+ses.speciesmpd+
                 Introduced_status+
                 SLA*Introduced_status+
                 Seed_mass*Introduced_status+
                 height*Introduced_status+
                 ses.speciesmpd*Introduced_status,
               na.action=na.pass,data=ukdata)

ukmodels<-model.avg(dredge(ukmod))
ukmod2<-dredge(ukmod)
beep(3)

ukmodsum<-summary(model.avg(ukmod2,subset=delta<4,fit=TRUE))

#get AIC importance values
ukmodsw<-sw(model.avg(ukmod2,subset=delta<4,fit=TRUE))


#-------------------------------------------------------------------------------

## JOIN AND PREP MODEL OUTPUTS ##

plotmod2 <-
  broom::tidy(plotmod) %>% filter(term != "(Intercept)"&term!="(phi)") %>% mutate(model = "Plot")
minormod2 <-
  broom::tidy(minormod) %>% filter(term != "(Intercept)"&term!="(phi)") %>% mutate(model = "Minor")
majormod2 <-
  broom::tidy(majormod) %>% filter(term != "(Intercept)"&term!="(phi)") %>% mutate(model = "Major")
sitemod2 <-
  broom::tidy(sitemod) %>% filter(term != "(Intercept)"&term!="(phi)") %>% mutate(model = "Site")
ukmod2 <-
  broom::tidy(ukmod) %>% filter(term != "(Intercept)"&term!="(phi)") %>% mutate(model = "UK")
ukmod2$term[ukmod2$term=="height"]<-"Height"
ukmod2$term[ukmod2$term=="height:Introduced_statusIntroduced"]<-"Height:Introduced_statusIntroduced"

#bind tidied models 
allmods<-rbind(plotmod2,minormod2,majormod2,sitemod2,ukmod2)

#rename wonky variables
allmods$term[allmods$term=="ses.speciesmpd"]<-"SES.MSPD"
allmods$term[allmods$term=="ses.speciesmpd:Introduced_statusIntroduced"]<-"SES.MSPD x Introduced"
allmods$term[allmods$term=="SLA:Introduced_statusIntroduced"]<-"SLA x Introduced"
allmods$term[allmods$term=="Height:Introduced_statusIntroduced"]<-"Height x Introduced"
allmods$term[allmods$term=="Seed_mass:Introduced_statusIntroduced"]<-"Seed mass x Introduced"
allmods$term[allmods$term=="Introduced_statusIntroduced"]<-"Introduced"
allmods$term[allmods$term=="Seed_mass"]<-"Seed mass"


#-------------------------------------------------------------------------------

## MAKE FIGURE ##

bigfig<-dwplot(allmods,dot_args = list(size=3),
               vline = geom_vline(
                 xintercept = 0,
                 colour = "grey60",
                 linetype = 2)) +
  scale_colour_viridis(option="D",discrete=TRUE,breaks = c("Plot", "Minor","Major","Site","UK"),
                       labels = c("Plot", "Minor","Major","Site","UK"))+
  theme_classic() + 
  # Setting `base_size` for fit the theme
  # No need to set `base_size` in most usage
  xlab("Coefficient Estimate") + ylab("") +
  geom_vline(xintercept = 0,
             colour = "grey60",
             linetype = 2) +
  theme(
    legend.position = "top",
    legend.justification = c(0, 0),
    legend.background = element_rect(colour = "grey60"),
    legend.title = element_blank())+
  theme(text = element_text(size=13))


bigfig

#-------------------------------------------------------------------------------


## GENERATE SCATTERPLOTS OF EFFECTS OF PHYLOGENETIC RELATEDNESS ##

# prepare data 
plantdata<-plantdata%>%filter(Introduced_status=="Native"|Introduced_status=="Introduced")

#change introduced_status to Status to make legend prettier
plantdata$Status<-plantdata$Introduced_status
sitedata$Status<-sitedata$Introduced_status
ukdata$Status<-ukdata$Introduced_status

#exclude pteridium aquilinum in budworth - makes all figures very ugly
plantdata<-plantdata%>%filter(ses.speciesmpd<100)



#-------------------------------------------------------------------------------

### PARTIAL REGRESSION SES.MSPD PLOTS ###

#- make model for all factors but focal fixed and interaction #

## PLOT ##

#create df with only rows with contain all values
plotdatanona<-plantdata%>%na.omit(Beta_contrib_plot,SLA,Seed_mass,Height,Introduced_status,ses.speciesmpd)
plotdatanona$Beta_contrib_plot<-plotdatanona$Beta_contrib_plot+0.00000000000000000001

#create model excluding ses.speciesmspd
plotmspdmod<-betareg(Beta_contrib_plot~
                   SLA+Seed_mass+
                   Height+
                   Introduced_status+
                   SLA*Introduced_status+
                   Seed_mass*Introduced_status+
                   Height*Introduced_status,
                 data=plotdatanona)
#save residuals
plotmspdresid<-resid(plotmspdmod)

#add residuals to df
plotdatanona$corrected.scbd<-plotmspdresid



## MINOR TRIAD ##

#create df with only rows with contain all values
minordatanona<-plantdata%>%na.omit(Beta_contrib_minor,SLA,Seed_mass,Height,Introduced_status,ses.speciesmpd)
minordatanona$Beta_contrib_minor<-plotdatanona$Beta_contrib_minor+0.00000000000000000001

#create model excluding ses.speciesmspd
minormspdmod<-betareg(Beta_contrib_minor~
                  SLA+Seed_mass+
                  Height+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                  Height*Introduced_status,
                data=minordatanona)
#save residuals
minormspdresid<-resid(minormspdmod)

#add residuals to df
minordatanona$corrected.scbd<-minormspdresid


## MAJOR TRIAD ##
#create df with only rows with contain all values
majordatanona<-plantdata%>%na.omit(Beta_contrib_major,SLA,Seed_mass,Height,Introduced_status,ses.speciesmpd)

#create model excluding ses.speciesmspd
majormspdmod<-betareg(Beta_contrib_major~
                  SLA+Seed_mass+
                  Height+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                  Height*Introduced_status,
                data=majordatanona)
#save residuals
majormspdresid<-resid(majormspdmod)

#add residuals to df
majordatanona$corrected.scbd<-majormspdresid


## SITE ##

#create df with only rows with contain all values
sitedatanona<-sitedata%>%na.omit(Beta_contrib_site,SLA,Seed_mass,Height,Introduced_status,ses.speciesmpd)

#create model excluding ses.speciesmspd
sitemspdmod<-betareg(beta_contrib~
                  SLA+Seed_mass+
                  Height+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                  Height*Introduced_status,
                data=sitedatanona)
#save residuals
sitemspdresid<-resid(sitemspdmod)

#add residuals to df
sitedatanona$corrected.scbd<-sitemspdresid


## UK ##

#create df with only rows with contain all values
uknona<-ukdata%>%na.omit(beta_contrib,SLA,Seed_mass,height,Introduced_status,ses.speciesmpd)

#create model excluding ses.speciesmspd
ukmspdmod<-betareg(beta_contrib~
                  SLA+Seed_mass+
                  height+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                height*Introduced_status,
                data=uknona)
#save residuals
ukmspdresid<-resid(ukmspdmod)

#add residuals to df
uknona$corrected.scbd<-ukmspdresid



#make plots
plotplot<-ggplot(data=plotdatanona,aes(x=log1p(ses.speciesmpd),y=(corrected.scbd),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab(expression(log(SES[MSPD])))+
  ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Plot")+ylim(-5,2)

minorplot<-ggplot(data=minordatanona,aes(x=log1p(ses.speciesmpd),y=(corrected.scbd),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab(expression(log(SES[MSPD])))+
  ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Minor triad")+ylim(-5,2)

majorplot<-ggplot(data=majordatanona,aes(x=log1p(ses.speciesmpd),y=(corrected.scbd),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab(expression(log(SES[MSPD])))+
  ylab("Corrected SCBD")+   
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Major triad")


siteplot<-ggplot(data=sitedatanona,aes(x=log1p(ses.speciesmpd),y=(corrected.scbd),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab(expression(log(SES[MSPD])))+
  ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Site")

ukplot<-ggplot(data=uknona,aes(x=log1p(ses.speciesmpd),y=(corrected.scbd),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+xlab(expression(log(SES[MSPD])))+
  ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("UK")


#join plots into one compound plot
ggarrange(plotplot, minorplot, majorplot, siteplot,ukplot,
          align='v', labels=NULL,
          common.legend = T,legend="bottom")


#-------------------------------------------------------------------------------

### PARTIAL REGRESSION HEIGHT PLOTS ###

#- make model for all factors but focal fixed and interaction #

#use dfs that contain no na values

## PLOT ##

#create model excluding height
plotheightmod<-betareg(Beta_contrib_plot~
                  SLA+Seed_mass+
                  ses.speciesmpd+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                  ses.speciesmpd*Introduced_status,
                data=plotdatanona)
#save residuals
plotheightresid<-resid(plotheightmod)

#add residuals to df
plotdatanona$corrected.scbdH<-plotheightresid



## MINOR TRIAD ##

#create model excluding height
minorheightmod<-betareg(Beta_contrib_minor~
                   SLA+Seed_mass+
                   ses.speciesmpd+
                   Introduced_status+
                   SLA*Introduced_status+
                   Seed_mass*Introduced_status+
                   ses.speciesmpd*Introduced_status,
                 data=minordatanona)
#save residuals
minorheightresid<-resid(minorheightmod)

#add residuals to df
minordatanona$corrected.scbdH<-minorheightresid


## MAJOR TRIAD ##

#create model excluding height
majorheightmod<-betareg(Beta_contrib_major~
                   SLA+Seed_mass+
                   ses.speciesmpd+
                   Introduced_status+
                   SLA*Introduced_status+
                   Seed_mass*Introduced_status+
                   ses.speciesmpd*Introduced_status,
                 data=majordatanona)
#save residuals
majorheightresid<-resid(majorheightmod)

#add residuals to df
majordatanona$corrected.scbdH<-majorheightresid


## SITE ##

#create model excluding height
siteheightmod<-betareg(beta_contrib~
                  SLA+Seed_mass+
                  ses.speciesmpd+
                  Introduced_status+
                  SLA*Introduced_status+
                  Seed_mass*Introduced_status+
                  ses.speciesmpd*Introduced_status,
                data=sitedatanona)
#save residuals
siteheightresid<-resid(siteheightmod)

#add residuals to df
sitedatanona$corrected.scbdH<-siteheightresid


## UK ##

#create model excluding height
ukheightmod<-betareg(beta_contrib~
                SLA+Seed_mass+
                ses.speciesmpd+
                Introduced_status+
                SLA*Introduced_status+
                Seed_mass*Introduced_status+
                ses.speciesmpd*Introduced_status,
              data=uknona)
#save residuals
ukheightresid<-resid(ukheightmod)

#add residuals to df
uknona$corrected.scbdH<-ukheightresid





## GENERATE SCATTERPLOTS OF EFFECTS OF HEIGHT ##

#make plots 

heightplotplot<-ggplot(data=plotdatanona,aes(x=log1p(Height),y=(corrected.scbdH),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab("log(Height (m))")+ylab("Corrected SCBD")+  
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Plot")+ylim(-4,2)

heightminorplot<-ggplot(data=minordatanona,aes(x=log1p(Height),y=(corrected.scbdH),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab("log(Height (m))")+ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Minor triad")+ylim(-5,2)

heightmajorplot<-ggplot(data=majordatanona,aes(x=log1p(Height),y=(corrected.scbdH),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab("log(Height (m))")+ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Major triad")


heightsiteplot<-ggplot(data=sitedatanona,aes(x=log1p(Height),y=(corrected.scbdH),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  geom_smooth(method="lm",formula=y~x)+
  theme_classic()+
  xlab("log(Height (m))")+ylab("Corrected SCBD")+ 
  guides(fill=guide_legend(title="Status"))+
  ggtitle("Site")

heightukplot<-ggplot(data=uknona,aes(x=log1p(height),y=(corrected.scbdH),col=Status))+
  geom_point(size=0.7)+
  scale_color_manual(values=c("#5ec962","#440154"))+
  theme_classic()+
  geom_smooth(method="lm",formula=y~x)+
  xlab("log(Height (m))")+ylab("Corrected SCBD")+  
  guides(fill=guide_legend(title="Status"))+
  ggtitle("UK")


#join plots into one compound plot
ggarrange(heightplotplot, heightminorplot, heightmajorplot, heightsiteplot,heightukplot,
          align='h', labels=NULL,
          common.legend = T,legend="bottom")

# end