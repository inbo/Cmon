###################################
#### Cmon 2024-2025 dataverwerking 
#### Bruno De Vos 


library(readxl)
library(writexl)
library(dplyr)
library(moments)   # for skewness function


setwd("C:/DATA/Cmon3")

### Inlezen laatste samengestelde tabel 


pathfilename<-"Cmon_voorraden_20250717_rapport4.xlsx"
PLATTAB<-read_excel(pathfilename,sheet=1, col_names=TRUE)
names(PLATTAB)
head(PLATTAB)

## controle LU cmon
PLATTAB[,c(2,62,151,152)]   # nieuwe classificatie Cmon (62) versus oude classificaties (151,152)
unique(PLATTAB$`Landgebruikscategorie Cmon`)

PLOT<-PLATTAB$Bodemlocaties_Naam 
## aantal proefvlakken
length(unique(PLOT))
### determine number of P, S and L plots
length(unique(PLOT[substr(PLOT,1,6)=="CMON_P"]))
length(unique(PLOT[substr(PLOT,1,6)=="CMON_S"]))
length(unique(PLOT[substr(PLOT,1,6)=="CMON_L"]))


### strooisellaag data
FFDATA<-PLATTAB[PLATTAB$`Bodemdiepteintervallen_Beschrijving`=="Strooisellaag",c(2,5,62,150,6,7,26,28,34,36,37,135,136)]
View(FFDATA)
FFDATA<-data.frame(FFDATA)
names(FFDATA)
#recode attribute names
names(FFDATA)<-c("PV","LAAG","LU","WERKJAAR","VAN","TOT","TC","TN","TC2","OLM","DIKTE","LCs","LNs") 
write.csv(FFDATA, "C:/DATA/Cmon3/AnalyseResultaten/Cmon_29_08_2025_FFstocks.csv")  ### beter 2025_07_17  !!!!


#FFDATA[FFDATA$Bodemlocaties_Naam=="CMON_S790faf0", c(1,10,11)]



View(PLATTAB)

unique(PLATTAB$Bodemlocaties_Auteurs)
#recode
PLATTAB$Bodemlocaties_Auteurs<-ifelse(PLATTAB$Bodemlocaties_Auteurs=="VO - Instituut voor Landbouw-; Visserij- en Voedingsonderzoek (ILVO)","ILVO",PLATTAB$Bodemlocaties_Auteurs)
PLATTAB$Bodemlocaties_Auteurs<-ifelse(PLATTAB$Bodemlocaties_Auteurs=="Instituut voor Landbouw-; Visserij- en Voedingsonderzoek (ILVO)","ILVO",PLATTAB$Bodemlocaties_Auteurs)
PLATTAB$Bodemlocaties_Auteurs<-ifelse(PLATTAB$Bodemlocaties_Auteurs=="VO - Instituut voor Natuur- en Bosonderzoek (INBO)","INBO",PLATTAB$Bodemlocaties_Auteurs)

PLATTAB.INBO<-PLATTAB[PLATTAB$Bodemlocaties_Auteurs=="INBO", ]    # selecteer alleen INBO data
PLATTAB.ILVO<-PLATTAB[PLATTAB$Bodemlocaties_Auteurs=="ILVO", ] 

#View(PLATTAB.INBO)
#View(PLATTAB.ILVO)

names(PLATTAB)


## checking PLATTAB

DF<-PLATTAB
dim(DF)
names(DF)

str(DF)


#### CHECK IF COL REFS ARE STILL RIGHT WHEN NEW TABLE IS LOADED !!! Problem with fieldnames with spaces

DF$PV<-DF$Bodemlocaties_Naam
DF$LG<-DF$'Landgebruikscategorie Cmon'   #Landgebruikscategorie Cmon 
#DF$SID<-DF$'Bodemdiepteintervallen_Naam'
#DF$MT<-DF$Bodemmonsters_Type
DF$LB72x<-DF$'Bodemlocaties_X_mL72'
DF$LB72y<-DF$'Bodemlocaties_Y_mL72'
DF$VAN<-DF$'Bodemmonsters_Diepte_van'
DF$TOT<-DF$'Bodemmonsters_Diepte_tot'
DF$ELEV<-DF$'Bodemlocaties_mv_mTAW'
DF$SID<-DF$Bodemmonsters_Bodemmonster
DF$TC<-DF[ ,26]   #TC
DF$TIC<-DF[ ,24]  #TIC
DF$TN<-DF[ ,28]   #"Totale N - percentage"
DF$BDfe<-DF[ ,29] #`Bulkdensiteit bodem < 2 mm vs totale volume - gemeten`
DF$BDt<-DF[ ,30]  #`Bulkdensiteit bodem totaal - gemeten`
DF$MCF<-DF[,31]        #Materiaal.groter.dan.1.cm.massaprocent
DF$pHKCl<-DF[ ,33]       #`pH KCl`
DF$Matrix<-DF[ ,41]            #`Bodemmonster type matrix`
DF$Laag<-DF$'Bodemdiepteintervallen_Beschrijving'
DF$TC.DC<-DF[ ,25]         #Totale.C...percentage.detectieconditie
DF$TIC.DC<-DF[ ,23]        #Anorganische.C...percentage.detectieconditie
DF$TN.DC<-DF[ ,27]        #Totale.N...percentage.detectieconditie
DF$OLM<-DF[ ,36]          # OLM in t/ha
DF$FFTHICK<-DF[ ,37]      # strooiseldikte
DF$MT<-DF$'Bodemmonsters_Type'

## recode DF lagen voor strooisel
#DF$Laag<-ifelse(DF$Laag %in%c("Staalnamelaag 0-10 cm","Staalnamelaag 10-30 cm","Staalnamelaag 30-60 cm","Staalnamelaag 60-100 cm"),DF$Laag,"Staalnamelaag Strooisel")
unique(DF$Laag)
str(DF)

###############################################
## basisstatistiek verkenning volledige dataset

# beschrijving dimensies dataset
dim(DF)
View(DF)
names(DF)

## aantal proefvlakken
length(unique(DF$PV))
### determine number of P, S and L plots
## aantal proefvlakken
### determine number of P, S and L plots
length(unique(DF$PV[substr(DF$PV,1,6)=="CMON_P"]))
length(unique(DF$PV[substr(DF$PV,1,6)=="CMON_S"]))
length(unique(DF$PV[substr(DF$PV,1,6)=="CMON_L"]))







## aantal geroerde stalen en ongeroerde stalen
DF$Bodemmonster.type.verstoring<-DF[ ,42]
length(DF$Bodemmonster.type.verstoring[DF$Bodemmonster.type.verstoring=="Geroerd"])
length(DF$Bodemmonster.type.verstoring[DF$Bodemmonster.type.verstoring=="Ongeroerd"])

## type matrix
DF$Bodemmonster.type.matrix<-DF[ ,41]
unique(DF$Bodemmonster.type.matrix)

length(DF$Bodemmonster.type.matrix[DF$Bodemmonster.type.matrix=="Mineraal"])
length(DF$Bodemmonster.type.matrix[DF$Bodemmonster.type.matrix=="Organomineraal"])
length(DF$Bodemmonster.type.matrix[DF$Bodemmonster.type.matrix=="nan"])
#DF$Bodemmonster.type.matrix<-ifelse(DF$Bodemmonster.type.matrix=="nan","Organisch",DF$Bodemmonster.type.matrix)
#length(DF$Bodemmonster.type.matrix[DF$Bodemmonster.type.matrix=="Organisch"])



### landgebruik Cmon
length(unique(DF$LG))
unique(DF$LG)

# aantal plots per landgebruik (all P,L and S plots)
length(unique(DF$PV[DF$LG=="Akker"]))
length(unique(DF$PV[DF$LG=="Blijvend grasland"]))
length(unique(DF$PV[DF$LG=="Bos"]))
length(unique(DF$PV[DF$LG=="Bos" & substr(DF$PV,1,6)=="CMON_P"]))   ### enkel P plots
length(unique(DF$PV[DF$LG=="Grasland buiten landbouw"]))
length(unique(DF$PV[DF$LG=="Niet-grasland open natuur"]))
length(unique(DF$PV[DF$LG=="Niet-grasland open natuur" & substr(DF$PV,1,6)=="CMON_P"]))   ### enkel P plots
length(unique(DF$PV[DF$LG=="Ruimtebeslag overige"]))
length(unique(DF$PV[DF$LG=="Ruimtebeslag tuinen"]))
length(unique(DF$PV[DF$LG=="Ruimtebeslag parken en recreatie"]))
length(unique(DF$PV[DF$LG=="Ruimtebeslag bermen"]))
length(unique(DF$PV[DF$LG=="nan"]))

## aantal monsters volgens monstertype
unique(DF$MT)
# mengmonsters
length(unique(DF$SID[DF$LG=="Akker"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Blijvend grasland"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Bos"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Grasland buiten landbouw"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Niet-grasland open natuur"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag parken en recreatie"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag bermen"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag tuinen"&DF$MT=="Mengmonster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag overige"&DF$MT=="Mengmonster"]))

# enkelvoudige monsters
length(unique(DF$SID[DF$LG=="Akker"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Blijvend grasland"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Bos"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Grasland buiten landbouw"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Niet-grasland open natuur"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag parken en recreatie"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag bermen"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag tuinen"&DF$MT=="Enkelvoudig monster"]))
length(unique(DF$SID[DF$LG=="Ruimtebeslag overige"&DF$MT=="Enkelvoudig monster"]))



### write DF
#write.csv2(DF, "C:/DATA/Cmon3/AnalyseResultaten/Cmon_DF_02_09_2025.csv", row.names = FALSE)
#write_xlsx(DF, "D:/TEMP/Cmon_DF_02_09_2025.xlsx",col_names = TRUE)

dim(DF)
names(DF)

### add missing data OLM (stimated from thickness)

DF$`Strooisellaag of viltlaag - massa`[DF$`Bodemlocaties_Naam`=="CMON_S6b2cf68" & DF$Laag=="Strooisellaag"& DF$MT=="Mengmonster"]<-24.2
DF$`Strooisellaag of viltlaag - massa`[DF$`Bodemlocaties_Naam`=="CMON_S790faf0" & DF$Laag=="Strooisellaag"& DF$MT=="Mengmonster"]<-51.8


#### HERE DF is complete => useful for other scripts
saveRDS(DF,"C:/DATA/Cmon3/DF.rds")
#####################################################

##############################################################################################################
##### AANMAKEN SUMMARY FILE CSTOCK BEREKENING

#### Variabelen
# PV proefvlak ID, LB72x LB72y  ELEV  LAAG  MATRIX  VAN  TOT  DIKTE  TC.DC, TC,  TIC,TIC.DC , TOC, TN.DC ,TN, BDfe.gem,  BDfe.sd, BDt, MCF 


## => data per PV en laag 
OUT<-NULL
PVlist<-unique(DF$PV)
nPVlist<-length(PVlist)
kolnaam<-c("PV","LB72x","LB72y","ELEV","LAAG","MATRIX","VAN","TOT","DIKTE","TC.DC","TC","TIC","TIC.DC","TOC","TN.DC","TN","BDfe.gem","BDfe.sd","BDt","MCF","OLM","LCs","LNs","CD","ND")
nkol<-length(kolnaam)
OUT<-array(NA, dim=c(nPVlist*5,nkol))   # velden per PV 4 dieptes + strooisel

kolnaam

tail(PVlist)


names(DF)
##################### ROUTINE
### loop
j=-4  # start rijteller bij 1

for (i in 1:nPVlist) {
  
  j=j+5   # rijteller
  
  PROF<-DF[DF$Bodemlocaties_Naam==PVlist[i], ]
  View(PROF)
  names(PROF)
  # proefvlak generiek
  PV<-unique(PROF$PV)
  LB72x<-unique(PROF$LB72x)
  LB72y<-unique(PROF$LB72y)
  ELEV<-unique(PROF$ELEV)

  # mengmonster data
 OUT[j,1]<-PV
 OUT[j,2]<-LB72x
 OUT[j,3]<-LB72y
 OUT[j,4]<-ELEV
 OUT[j,5]<-"Staalnamelaag 0-10 cm"
 OUT[j,6]<-"Mineraal"
 OUT[j,7]<-PROF$VAN[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"]
 OUT[j,8]<-PROF$TOT[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"]
 OUTVAN<-as.numeric(OUT[j,7])
 OUTTOT<- as.numeric(OUT[j,8])
 OUT[j,9]<- OUTTOT-OUTVAN   # Dikte
 LAAGDIKTE<- as.numeric(OUT[j,9])
 OUT[j,10]<-PROF$TC.DC[PROF$Laag=="Staalnamelaag 0-10 cm" & PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j,11]<-PROF$TC[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j,12]<-PROF$TIC.DC[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUT[j,13]<-PROF$TIC[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUTTC<-as.numeric(OUT[j,11])
 OUTTIC.DC<- OUT[j,12]
 OUTTIC<-as.numeric(OUT[j,13])
 OUTTOC<-ifelse(OUTTIC.DC=="LT"&!is.na(OUTTIC.DC),OUTTC,OUTTC-OUTTIC)   # corrigeren voor TIC  => TOC=TC-TIC
 LAAGTOC<-as.numeric(OUTTOC)
 OUT[j,14]<-OUTTOC
 OUT[j,15]<-PROF$TN.DC[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 OUT[j,16]<-PROF$TN[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 LAAGTN<-as.numeric(OUT[j,16])
 OUT[j,17]<-round(mean(PROF$BDfe[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 LAAGBD<-as.numeric(OUT[j,17])
 OUT[j,18]<-round(sd(PROF$BDfe[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 OUT[j,19]<-round(mean(PROF$BDt[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDt)],na.rm=T),3)
 OUT[j,20]<-round(mean(PROF$MCF[PROF$Laag=="Staalnamelaag 0-10 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$MCF)],na.rm=T),3)
 OUT[j,21]<-NA
 OUT[j,22]<-round((LAAGDIKTE*LAAGBD*LAAGTOC),2)    # LCs layer Carbon stock (t/ha)
 OUT[j,23]<-round((LAAGDIKTE*LAAGBD*LAAGTN),2)    # LNsLayer N stock (t/ha)
 LCs<-as.numeric(OUT[j,22])
 LNs<-as.numeric(OUT[j,23])
 OUT[j,24]<-round(LCs/LAAGDIKTE,3)   # CD   Carbon density in t/ha per cm
 OUT[j,25]<-round(LNs/LAAGDIKTE,3)   # ND   Nitrogen density in t/ha per cm
 
 
#layer 2
 if ("Staalnamelaag 10-30 cm" %in% PROF$Laag)  {    #next   # skip this layer if no such layer
 OUT[j+1,1]<-PV
 OUT[j+1,2]<-LB72x
 OUT[j+1,3]<-LB72y
 OUT[j+1,4]<-ELEV
 OUT[j+1,5]<-"Staalnamelaag 10-30 cm"
 OUT[j+1,6]<-"Mineraal"
 OUT[j+1,7]<-PROF$VAN[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"]
 OUT[j+1,8]<-PROF$TOT[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"]
 OUTVAN<-as.numeric(OUT[j+1,7])
 OUTTOT<-as.numeric(OUT[j+1,8])
 OUT[j+1,9]<- OUTTOT-OUTVAN   # Dikte
 LAAGDIKTE<- as.numeric(OUT[j+1,9])
 OUT[j+1,10]<-PROF$TC.DC[PROF$Laag=="Staalnamelaag 10-30 cm" & PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+1,11]<-PROF$TC[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+1,12]<-PROF$TIC.DC[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUT[j+1,13]<-PROF$TIC[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUTTC<-as.numeric(OUT[j+1,11])
 OUTTIC.DC<- OUT[j+1,12]
 OUTTIC<-as.numeric(OUT[j+1,13])
 OUTTOC<-ifelse(OUTTIC.DC=="LT"&!is.na(OUTTIC.DC),OUTTC,OUTTC-OUTTIC)   # corrigeren voor TIC  => TOC=TC-TIC
 LAAGTOC<-as.numeric(OUTTOC)
 OUT[j+1,14]<-OUTTOC
 OUT[j+1,15]<-PROF$TN.DC[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 OUT[j+1,16]<-PROF$TN[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 LAAGTN<-as.numeric(OUT[j+1,16])
 OUT[j+1,17]<-round(mean(PROF$BDfe[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 LAAGBD<-as.numeric(OUT[j+1,17])
 OUT[j+1,18]<-round(sd(PROF$BDfe[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 OUT[j+1,19]<-round(mean(PROF$BDt[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDt)],na.rm=T),3)
 OUT[j+1,20]<-round(mean(PROF$MCF[PROF$Laag=="Staalnamelaag 10-30 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$MCF)],na.rm=T),3)
 OUT[j+1,21]<-NA
 OUT[j+1,22]<-round((LAAGDIKTE*LAAGBD*LAAGTOC),2)    # LCs layer Carbon stock (t/ha)
 OUT[j+1,23]<-round((LAAGDIKTE*LAAGBD*LAAGTN),2)    # LNsLayer N stock (t/ha)
 LCs<-as.numeric(OUT[j+1,22])
 LNs<-as.numeric(OUT[j+1,23])
 OUT[j+1,24]<-round(LCs/LAAGDIKTE,3)   # CD   Carbon density in t/ha per cm
 OUT[j+1,25]<-round(LNs/LAAGDIKTE,3)   # ND   Nitrogen density in t/ha per cm
 }
 
 #layer 3
 if ("Staalnamelaag 30-60 cm" %in% PROF$Laag)  {    #next   # skip this layer if no such layer
 OUT[j+2,1]<-PV
 OUT[j+2,2]<-LB72x
 OUT[j+2,3]<-LB72y
 OUT[j+2,4]<-ELEV
 OUT[j+2,5]<-"Staalnamelaag 30-60 cm"
 OUT[j+2,6]<-"Mineraal"
 OUT[j+2,7]<-PROF$VAN[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"]
 OUT[j+2,8]<-PROF$TOT[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"]
 OUTVAN<-as.numeric(OUT[j+2,7])
 OUTTOT<-as.numeric(OUT[j+2,8])
 OUT[j+2,9]<- OUTTOT-OUTVAN   # Dikte
 LAAGDIKTE<- as.numeric(OUT[j+2,9])
 OUT[j+2,10]<-PROF$TC.DC[PROF$Laag=="Staalnamelaag 30-60 cm"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+2,11]<-PROF$TC[PROF$Laag=="Staalnamelaag 30-60 cm"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+2,12]<-PROF$TIC.DC[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUT[j+2,13]<-PROF$TIC[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUTTC<-as.numeric(OUT[j+2,11])
 OUTTIC.DC<- OUT[j+2,12]
 OUTTIC<-as.numeric(OUT[j+2,13])
 OUTTOC<-ifelse(OUTTIC.DC=="LT"&!is.na(OUTTIC.DC),OUTTC,OUTTC-OUTTIC)   # corrigeren voor TIC  => TOC=TC-TIC
 LAAGTOC<-as.numeric(OUTTOC)
 OUT[j+2,14]<-OUTTOC
 OUT[j+2,15]<-PROF$TN.DC[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 OUT[j+2,16]<-PROF$TN[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 LAAGTN<-as.numeric(OUT[j+2,16])
 OUT[j+2,17]<-round(mean(PROF$BDfe[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 LAAGBD<-as.numeric(OUT[j+2,17])
 OUT[j+2,18]<-round(sd(PROF$BDfe[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 OUT[j+2,19]<-round(mean(PROF$BDt[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDt)],na.rm=T),3)
 OUT[j+2,20]<-round(mean(PROF$MCF[PROF$Laag=="Staalnamelaag 30-60 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$MCF)],na.rm=T),3)
 OUT[j+2,21]<-NA
 OUT[j+2,22]<-round((LAAGDIKTE*LAAGBD*LAAGTOC),2)    # LCs layer Carbon stock (t/ha)
 OUT[j+2,23]<-round((LAAGDIKTE*LAAGBD*LAAGTN),2)    # LNsLayer N stock (t/ha)
 LCs<-as.numeric(OUT[j+2,22])
 LNs<-as.numeric(OUT[j+2,23])
 OUT[j+2,24]<-round(LCs/LAAGDIKTE,3)   # CD   Carbon density in t/ha per cm
 OUT[j+2,25]<-round(LNs/LAAGDIKTE,3)   # ND   Nitrogen density in t/ha per cm
 }
 
 #names(PROF)
 #PROF[ ,c(139,130,140,131,123)]
 
 #layer 4
 if ("Staalnamelaag 60-100 cm" %in% PROF$Laag)  {    #next   # skip this layer if no such layer
 OUT[j+3,1]<-PV
 OUT[j+3,2]<-LB72x
 OUT[j+3,3]<-LB72y
 OUT[j+3,4]<-ELEV
 OUT[j+3,5]<-"Staalnamelaag 60-100 cm"
 OUT[j+3,6]<-"Mineraal"
 OUT[j+3,7]<-PROF$VAN[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"]
 OUT[j+3,8]<-PROF$TOT[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"]
 OUTVAN<-as.numeric(OUT[j+3,7])
 OUTTOT<-as.numeric(OUT[j+3,8])
 OUT[j+3,9]<- OUTTOT-OUTVAN   # Dikte
 LAAGDIKTE<- as.numeric(OUT[j+3,9])
 OUT[j+3,10]<-PROF$TC.DC[PROF$Laag=="Staalnamelaag 60-100 cm"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+3,11]<-PROF$TC[PROF$Laag=="Staalnamelaag 60-100 cm"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+3,12]<-PROF$TIC.DC[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUT[j+3,13]<-PROF$TIC[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TIC)]
 OUTTC<-as.numeric(OUT[j+3,11])
 OUTTIC.DC<- OUT[j+3,12]
 OUTTIC<-as.numeric(OUT[j+3,13])
 OUTTOC<-ifelse(OUTTIC.DC=="LT"&!is.na(OUTTIC.DC),OUTTC,OUTTC-OUTTIC)   # corrigeren voor TIC  => TOC=TC-TIC
 LAAGTOC<-as.numeric(OUTTOC)
 OUT[j+3,14]<-OUTTOC
 OUT[j+3,15]<-PROF$TN.DC[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 OUT[j+3,16]<-PROF$TN[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 LAAGTN<-as.numeric(OUT[j+3,16])
 OUT[j+3,17]<-round(mean(PROF$BDfe[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 LAAGBD<-as.numeric(OUT[j+3,17])
 OUT[j+3,18]<-round(sd(PROF$BDfe[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDfe)],na.rm=T),3)
 OUT[j+3,19]<-round(mean(PROF$BDt[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$BDt)],na.rm=T),3)
 OUT[j+3,20]<-round(mean(PROF$MCF[PROF$Laag=="Staalnamelaag 60-100 cm"&PROF$MT=="Enkelvoudig monster"&!is.na(PROF$MCF)],na.rm=T),3)
 OUT[j+3,21]<-NA
 OUT[j+3,22]<-round((LAAGDIKTE*LAAGBD*LAAGTOC),2)    # LCs layer Carbon stock (t/ha)
 OUT[j+3,23]<-round((LAAGDIKTE*LAAGBD*LAAGTN),2)    # LNsLayer N stock (t/ha)
 LCs<-as.numeric(OUT[j+3,22])
 LNs<-as.numeric(OUT[j+3,23])
 OUT[j+3,24]<-round(LCs/LAAGDIKTE,3)   # CD   Carbon density in t/ha per cm
 OUT[j+3,25]<-round(LNs/LAAGDIKTE,3)   # ND   Nitrogen density in t/ha per cm
 }
 
 #layer 5 STROOISEL /ORGANISCH
 if ("Strooisellaag" %in% PROF$Laag)  {    #next   # skip this layer if no FF
 OUT[j+4,1]<-PV
 OUT[j+4,2]<-LB72x
 OUT[j+4,3]<-LB72y
 OUT[j+4,4]<-ELEV
 OUT[j+4,5]<-"Strooisellaag"
 OUT[j+4,6]<-"Organisch"
 OUT[j+4,7]<-PROF$VAN[PROF$Laag=="Strooisellaag"&PROF$MT=="Mengmonster"]
 OUT[j+4,8]<-PROF$TOT[PROF$Laag=="Strooisellaag"&PROF$MT=="Mengmonster"]
 OUTVAN<-as.numeric(OUT[j+4,7])
 OUTTOT<-as.numeric(OUT[j+4,8])
 OUT[j+4,9]<- OUTTOT-OUTVAN   # Dikte in cm
 FFDikte<-as.numeric(OUT[j+4,9])
 OUT[j+4,10]<-"NA"
 OUT[j+4,11]<-PROF$TC[PROF$Laag=="Strooisellaag"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 OUT[j+4,12]<-"NA"
 OUT[j+4,13]<-0
 OUT[j+4,14]<-PROF$TC[PROF$Laag=="Strooisellaag"& PROF$MT=="Mengmonster"&!is.na(PROF$TC)]
 FFTOC<-as.numeric(OUT[j+4,14])    # TOC van strooisellaag
 OUT[j+4,15]<-"NA"
 OUT[j+4,16]<-PROF$TN[PROF$Laag=="Strooisellaag"& PROF$MT=="Mengmonster"&!is.na(PROF$TN)]
 FFTN<- as.numeric(OUT[j+4,16])    #TN van strooisellaag
 OUT[j+4,17]<-NA
 OUT[j+4,18]<-NA
 OUT[j+4,19]<-NA
 OUT[j+4,20]<-NA 
 OUT[j+4,21]<-0    ## PROF$OLM[PROF$Laag=="Strooisellaag"& PROF$MT=="Mengmonster" &!is.na(PROF$OLM)]    ### strooiselmassa in ton/ha
 OLM<-as.numeric(OUT[j+4,21])
 OUT[j+4,22]<-round((OLM*(FFTOC/100)),2)    # LCs layer Carbon stock (t/ha)
 OUT[j+4,23]<-round((OLM*(FFTN/100)),2)    # LNsLayer N stock (t/ha)
 LCs<-as.numeric(OUT[j+4,22])
 LNs<-as.numeric(OUT[j+4,23])
 OUT[j+4,24]<-round(LCs/FFDikte,3)   # CD   Carbon density in t/ha per cm
 OUT[j+4,25]<-round(LNs/FFDikte,3)   # ND   Nitrogen density in t/ha per cm
 }
 
 View(PROF)
 next
} 
DF.OUT<-data.frame(OUT)
names(DF.OUT)<-kolnaam
DF.OUT

names(DF.OUT)



######################################################

View(DF.OUT)

length(unique(DF.OUT$PV))



DF2<-DF.OUT[complete.cases(DF.OUT[,c(1:6)]), ]
View(DF2)
length(unique(DF2$PV))
### determine number of P, S and L plots
length(unique(DF2$PV[substr(DF2$PV,1,6)=="CMON_P"]))
length(unique(DF2$PV[substr(DF2$PV,1,6)=="CMON_S"]))
length(unique(DF2$PV[substr(DF2$PV,1,6)=="CMON_L"]))


write.csv(DF2, "C:/DATA/Cmon3/AnalyseResultaten/Cmon_02_09_2025_Layer_Stockcalc.csv", row.names = FALSE)
dim(DF2)
names(DF2)

######################################################################################################################
######################################################################################################################
### strooiseloverzicht
names(DF2)

names(FFDATA)
               

View(FFDATA)
dim(FFDATA)  # 146 strooisellagen in all plots P,S,L
tapply(FFDATA$PV, FFDATA$LU, length)


#FFDATA<-read.csv("C:/DATA/Cmon3/AnalyseResultaten/Cmon_29_08_2025_FFstocks.csv")


## opvragen plotinfo FF - missing info
FFDATA[FFDATA$PV %in% c("CMON_P6b2cf68","CMON_S6b2cf68"), ]
FFDATA[FFDATA$PV %in% c("CMON_P790faf0","CMON_S790faf0"), ]


## enkel FFDATA in P plots
FFDATA.P<-FFDATA[substr(FFDATA$PV,1,6)=="CMON_P", ]
dim(FFDATA.P)
tapply(FFDATA.P$PV, FFDATA.P$LU, length)

# summary strooiseldata (enkel P plots)
FFCs<-as.numeric(FFDATA.P$LCs)
summary(FFCs)
nOb<-length(FFCs)
gem<-round(mean(FFCs),1) ;gem
med<-round(median(FFCs),1)
skw<-round(skewness(FFCs,na.rm=T),2)

mean(FFCs[FFDATA.P$LU=="Bos"])



### print in graph

hist(FFCs, nclass=40, probability = TRUE, xlab="C stock (t/ha)", ylab= "Probabiliteit van voorkomen", 
     main="Histogram organische koolstofvoorraad in strooisellagen alle landgebruiken")
text(35,0.05,paste0("Aantal observaties = ",nOb,""), pos=4)
text(35,0.047,paste0("Gemiddelde = ",gem," t C/ha"), pos=4)
text(35,0.044,paste0("Mediaan = ",med," t C/ha"), pos=4)
text(35,0.041,paste0("Scheefheid = ",skw), pos=4)

lines(density(FFCs), lwd = 2, col = "red")


plot(FFDATA.P$DIKTE, FFDATA.P$LCs)

mod<-lm(FFDATA.P$LCs~-1+FFDATA.P$DIKTE)
summary(mod)
abline(mod)

mod2<-lm(FFDATA.P$OLM~-1+FFDATA.P$DIKTE)
summary(mod2)
abline(mod2)



# summary strooiseldata N stock
FFNs<-as.numeric(FFDATA.P$LNs)
summary(FFNs)
nOb<-length(FFNs)
gem<-round(mean(FFNs),2)
med<-round(median(FFNs),2)
skw<-round(skewness(FFNs,na.rm=T),2)

### print in graph

hist(FFNs, nclass=40, probability = TRUE, xlab="N stock (t/ha)", ylab= "Probabiliteit van voorkomen", 
     main="Histogram stikstofvoorraad in strooisellagen alle landgebruiken")
text(1.7,1.5,paste0("Aantal observaties = ",nOb,""), pos=4)
text(1.7,1.4,paste0("Gemiddelde = ",gem," t N/ha"), pos=4)
text(1.7,1.3,paste0("Mediaan = ",med," t N/ha"), pos=4)
text(1.7,1.2,paste0("Scheefheid = ",skw), pos=4)

lines(density(FFNs), lwd = 2, col = "red")


## analyse per werkjaar voor strooisellagen in bos

FFDATA.P.Bos<-FFDATA.P[FFDATA.P$LU=="Bos", ]

names(FFDATA.P.Bos)

out<-boxplot(FFDATA.P.Bos$LCs~FFDATA.P.Bos$WERKJAAR, notch=TRUE, col="#487524",
        xlab="Werkjaar", ylab="Koolstofvoorraad 0-30 cm in strooisellagen van bos")
tapply(FFDATA.P.Bos$LCs,FFDATA.P.Bos$WERKJAAR, length)
text(c(1,2,3,4), c(30,30,30,30),out$n)

#------------------------------------------


names(DF2)
DF2$MATRIX

DF2<-type.convert(DF2, as.is=TRUE)
str(DF2)


summary(DF2$TOC)

tapply(DF2$TOC,list(DF2$MATRIX,DF2$LAAG),length)
tapply(TOC,list(MATRIX,LAAG),mean,na.rm=T)
tapply(TOC,list(MATRIX,LAAG),sd,na.rm=T)

tapply(TOC,list(MATRIX,LAAG),min,na.rm=T)
tapply(TOC,list(MATRIX,LAAG),max,na.rm=T)

hist(TOC, nclass=40, freq=FALSE, xlab="TOC (%)", main="Organische koolstof INBO data")

summary(TIC)

tapply(TIC,list(MATRIX,LAAG),length)
tapply(TIC,list(MATRIX,LAAG),mean,na.rm=T)
tapply(TIC,list(MATRIX,LAAG),sd,na.rm=T)

tapply(TIC,list(MATRIX,LAAG),min,na.rm=T)
tapply(TIC,list(MATRIX,LAAG),max,na.rm=T)

plot(pHKCl,TIC)
plot(TOC,TIC)

hist(TIC)
#---------------
summary(BDfe)

tapply(BDfe,list(MATRIX,LAAG),length)
tapply(BDfe,list(MATRIX,LAAG),mean,na.rm=T)
tapply(BDfe,list(MATRIX,LAAG),sd,na.rm=T)

tapply(BDfe,list(MATRIX,LAAG),min,na.rm=T)
tapply(BDfe,list(MATRIX,LAAG),max,na.rm=T)

hist(BDfe)




#-----------------------
### read in Cummulatieve stockberekening
PROFmps<-read.csv("C:/DATA/Cmon2/AnalyseResultaten/Cmon_16_07_2024_Cumm_CN_voorraad.csv")
names(DF)

FRONTEND<-distinct(DF[ ,c(4,14,124,125,59,62,64)])   # selectie van verklarende variabelen
names(FRONTEND)


### combine FRONTEND met PROFmps
names(PROFmps)
names(FRONTEND)
dim(FRONTEND)

FRONTPROFmps<-left_join(FRONTEND,PROFmps, join_by("Bodemlocaties_Naam" == "PLOTNAAM") ) 

## combineer met FFstocks
names(FRONTPROFmps)
names(FFDATA)


FRONTPROFmps2<-left_join(FRONTPROFmps,FFDATA, join_by("Bodemlocaties_Naam" == "PV") ) 
View(FRONTPROFmps2)

write.csv(FRONTPROFmps2,"C:/DATA/Cmon2/AnalyseResultaten/FRONTPROFmps.csv", row.names = FALSE)

unique(FRONTPROFmps2$LAAG)
names(FRONTPROFmps2)
tapply(FRONTPROFmps2$LAAG[FRONTPROFmps2$LAAG=="Strooisellaag"], FRONTPROFmps2$'Landgebruikscategorie Cmon'[FRONTPROFmps2$LAAG=="Strooisellaag"], length)

########################
## ANALYSE C en N STOCKS

View(DF2)   ## data op sample niveau

DF3<-FRONTPROFmps2    ## data op plot niveau
names(DF3)

View(DF3)




### Over alle landuses

#### Topsoil stock  (0-30 cm)
PCsmps30<-na.omit(DF3$PCsmps30)

length(PCsmps30)
summary(PCsmps30)

hist(PCsmps30, nclass=20, freq=TRUE, xlab="Koolstofvoorraad 0-30 cm  (t/ha)", main="Histogram koolstofvoorraad 0-30 cm alle landgebruiken" )
gem<-round(mean(PCsmps30),2)
med<-round(median(PCsmps30),2)
skw<-round(skewness(PCsmps30,na.rm=T),2)
### print in graph
text(150,40,paste0("Gemiddelde = ",gem," t C/ha"), pos=4, col="red")
text(150,35,paste0("Mediaan = ",med," t C/ha"), pos=4, col="green")
text(150,30,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")




#### Totale stock  (0-100 cm)
PCsmps100<-na.omit(DF3$PCsmps100)

length(PCsmps100)
summary(PCsmps100)

hist(PCsmps100, nclass=30, freq=TRUE, xlab="Koolstofvoorraad in minerale bodem   (t/ha)", main="Histogram koolstofvoorraad 0-100 cm alle landgebruiken" )
gem<-round(mean(PCsmps100),1)
med<-round(median(PCsmps100),1)
skw<-round(skewness(PCsmps100,na.rm=T),2)
### print in graph
text(500,50,paste0("Gemiddelde = ",gem," t C/ha"), pos=4, col="red")
text(500,40,paste0("Mediaan = ",med," t C/ha"), pos=4, col="green")
text(500,30,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")



#### Cs30/Cs100 ratio

RATCs30Cs100<-(PCsmps30/PCsmps100)


length(RATCs30Cs100)
summary(RATCs30Cs100)

hist(RATCs30Cs100, nclass=30, freq=TRUE, xlab="Ratio 30cm C-voorraad tot 100 cm voorraad", main="Histogram ratio 0-30 cm stock tov 0-100 cm stock alle landgebruiken" )
gem<-round(mean(RATCs30Cs100),2)
med<-round(median(RATCs30Cs100),2)
skw<-round(skewness(RATCs30Cs100,na.rm=T),2)
### print in graph
text(0.2,20,paste0("Gemiddelde = ",gem,""), pos=4, col="red")
text(0.2,17,paste0("Mediaan = ",med,""), pos=4, col="green")
text(0.2,14,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")






#########################
### Stikstof



### Over alle landuses

#### Topsoil stock  (0-30 cm)
PNsmps30<-na.omit(DF3$PNsmps30)

length(PNsmps30)
summary(PNsmps30)

hist(PNsmps30, nclass=20, freq=TRUE, xlab="Stikstofvoorraad 0-30 cm  (t/ha)", main="Histogram stikstofvoorraad 0-30 cm alle landgebruiken" )
gem<-round(mean(PNsmps30),2)
med<-round(median(PNsmps30),2)
skw<-round(skewness(PNsmps30,na.rm=T),2)
### print in graph
text(10,40,paste0("Gemiddelde = ",gem," t N/ha"), pos=4, col="red")
text(10,30,paste0("Mediaan = ",med," t N/ha"), pos=4, col="green")
text(10,20,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")







##################################
### Bijmengingen in plots

names(DF)
str(DF)

# antropogeen
ANTRO<-DF[!is.na(DF$Percentage.steken.met.aanwezigheid.antropogeen.materiaal), c(60, 4:7,24,25,27,36:38,61)]
View(distinct(ANTRO))


VEEN<-DF[!is.na(DF$Percentage.steken.met.aanwezigheid.veen)& DF$Percentage.steken.met.aanwezigheid.veen>0, c(60, 4:7,24,25,27,36:38,67)]
View(distinct(VEEN))
## enkel CMON_P90c80d7 => bos, niet zo grote C stock...

BC<-DF[!is.na(DF$Percentage.steken.met.aanwezigheid.houtskool)& DF$Percentage.steken.met.aanwezigheid.houtskool>0, c(60, 4:7,24,25,27,36:38,64)]
View(distinct(BC))
## enkel CMON_P90c80d7 => bos, niet zo grote C stock...





#### Totale stock  (0-100 cm)
PNsmps100<-na.omit(DF3$PNsmps100)

length(PNsmps100)
summary(PNsmps100)

hist(PNsmps100, nclass=30, freq=TRUE, xlab="Stikstofvoorraad in minerale bodem   (t/ha)", main="Histogram stikstofvoorraad 0-100 cm alle landgebruiken" )
gem<-round(mean(PNsmps100),1)
med<-round(median(PNsmps100),1)
skw<-round(skewness(PNsmps100,na.rm=T),2)
### print in graph
text(30,40,paste0("Gemiddelde = ",gem," t N/ha"), pos=4, col="red")
text(30,30,paste0("Mediaan = ",med," t N/ha"), pos=4, col="green")
text(30,20,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")



#### Ns30/Ns100 ratio

RATNs30Ns100<-(PNsmps30/PNsmps100)


length(RATNs30Ns100)
summary(RATNs30Ns100)

hist(RATNs30Ns100, nclass=30, freq=TRUE, xlab="Ratio 30cm N-voorraad tot 100 cm voorraad", main="Histogram ratio 0-30 cm stikstof stock tov 0-100 cm stock alle landgebruiken" )
gem<-round(mean(RATNs30Ns100),2)
med<-round(median(RATNs30Ns100),2)
skw<-round(skewness(RATNs30Ns100,na.rm=T),2)
### print in graph
text(0.25,30,paste0("Gemiddelde = ",gem,""), pos=4, col="red")
text(0.25,25,paste0("Mediaan = ",med,""), pos=4, col="green")
text(0.25,20,paste0("Scheefheid = ",skw), pos=4)
abline(v=gem, lty=2, col="red")
abline(v=med, lty=2, col="green")





###### PER LANDGEBRUIK Cs100
names(DF3)

tapply(DF3$PCsmps100,DF3$'Landgebruikscategorie Cmon',length) 
tapply(DF3$PCsmps100,DF3$'Landgebruikscategorie Cmon',summary,na.rm=T) 
Cs100SD<-tapply(DF3$PCsmps100,DF3$'Landgebruikscategorie Cmon',sd,na.rm=T) 
round(Cs100SD,1)
Cs100Mean<-tapply(DF3$PCsmps100,DF3$'Landgebruikscategorie Cmon',mean,na.rm=T) 
Cs100CV<-round((100*Cs100SD)/Cs100Mean,1)
Cs100CV



###### PER LANDGEBRUIK Ns100
names(DF3)

tapply(DF3$PNsmps100,DF3$'Landgebruikscategorie Cmon',length) 
tapply(DF3$PNsmps100,DF3$'Landgebruikscategorie Cmon',summary,na.rm=T) 

Ns100SD<-tapply(DF3$PNsmps100,DF3$'Landgebruikscategorie Cmon',sd,na.rm=T) 
round(Ns100SD,1)
Ns100Mean<-tapply(DF3$PNsmps100,DF3$'Landgebruikscategorie Cmon',mean,na.rm=T) 
Ns100CV<-round((100*Ns100SD)/Ns100Mean,1)
Ns100CV





#### Cs versus Ns
names(DF3)

unique(DF3$'Landgebruikscategorie Cmon')
DF3$LUCode<-DF3$'Landgebruikscategorie Cmon'
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Akker",1,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Blijvend grasland",2,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Bos",3,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Natuur",4,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Ruimtebeslag tuinen",5,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Ruimtebeslag parken en recreatiedomeinen",6,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Ruimtebeslag bermen",7,DF3$LUCode)
DF3$LUCode<-ifelse(DF3$'Landgebruikscategorie Cmon'=="Ruimtebeslag overige",8,DF3$LUCode)
unique(DF3$LUCode)
DF3$LUCode<-as.numeric(DF3$LUCode)



plot(DF3$PCsmps100,DF3$PNsmps100, pch=16, col=DF3$LUCode, xlab="Cs100 stock  (t/ha)"
     , ylab="Ns100 stock   (t/ha)", main="Cs100/Ns100 relatie volgens landgebruik")
legend(500,20,c("Akker","Blijvend Grasland","Bos","Natuur","Residentieel"), col=c(1,2,3,4,5,6,7,8), pch=16)
lines(lowess(PCsmps100,PNsmps100))

plot(DF3$PCsmps30,DF3$PNsmps30, pch=16, col=DF3$LUCode, xlab="Cs30 stock  (t/ha)"
     , ylab="Ns30 stock   (t/ha)", main="Cs100/Ns100 relatie volgens landgebruik")
legend(500,20,c("Akker","Perm.Grasland","Bos","Natuur","Residentieel"), col=c(1,2,3,4,5), pch=16)
lines(lowess(PCsmps30,PNsmps30))







#### DF extract

names(DF)
unique(DF$'Landgebruikscategorie Cmon')

DF.RB<-DF[DF$'Landgebruikscategorie Cmon'%in%c("Ruimtebeslag overige","Ruimtebeslag tuinen","Ruimtebeslag bermen",
                                                  "Ruimtebeslag parken en recreatiedomeinen") & DF$Bodemmonsters_Type=="Mengmonster" & 
                                                  DF$Bodemmonsters_Diepte_tot<=30, c(4:12,14,22,23,62:64)]
dim(DF.RB)
View(DF.RB)

write.csv2(DF.RB, "C:/DATA/Cmon2/RuBBio/Cmon_Ruimtebeslag_stalen_20250827csv")