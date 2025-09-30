#### Mass preserving spline / Equal area spline function of soil layers/horizons
#### Generator of spline functions of Carbon density with depth (0-100cm)
##### PROFILES GENERATOR via mpsplines

### RUN SOILSPLINE function first !!!!
library(dplyr)
library(bootstrap)

##############################################
### FUNCTIONS BASED ON SELECTED PROFILE ######
##############################################
CDmatrix<-array(data=0, dim=100)


Csmpsplinematrix<-function(PROF) {


PLOTNAME<-paste(unique(PROF$PROFID))
LTOP<-PROF$TOP
LBOT<-PROF$BOT
LCd<-PROF$CD
nlay<-length(PROF$CD)
maxBOT<-SOILDEPTH
proflayCs<-array(0,100)

 
# fit a spline if at least 2 horizons:
if  (length(LCd)>=2) {
out<-SOILSPLINE(id=PLOTNAME,LTOP,LBOT,LCd,msd=100,graph=FALSE)
proflayCs<-data.frame(out)
names(proflayCs)<-PLOTNAME
}

return(proflayCs)
}
 


#########################################################################
########################### CALCULATION FOR Cmon DATASET  ###############
#########################################################################


######################################
## Loading DATASETS from CSV files  ##
######################################

### inlezen "C:/DATA/Cmon/AnalyseResultaten/Cmon_10_05_2023_Stockcalc.csv"
SOILLAYCs<-read.csv("C:/DATA/Cmon3/AnalyseResultaten/Cmon_02_09_2025_Layer_Stockcalc.csv", header=TRUE)
length(unique(SOILLAYCs$PV))

View(SOILLAYCs)

### enkel SOILLAYCs van P plots (monitoring plots, dus niet van L en S plots)
SOILLAYCs<-SOILLAYCs[substr(SOILLAYCs$PV,1,6)=="CMON_P", ]
length(unique(SOILLAYCs$PV))

# readXL
#library(readxl)
#SOILLAYCs<-read_excel("C:\\R_scripts\\R_FSCstocks\\FS.Carbon.KUL.min3.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "",skip = 0)



dim(SOILLAYCs)
names(SOILLAYCs)
View(SOILLAYCs)

## toevoegen gemiddelde diepte

SOILLAYCs$GEM.DIEPTE<-(SOILLAYCs$VAN+SOILLAYCs$TOT)/2

### Alleen minerale bodems, geen strooisellagen
SOILLAYCs<-SOILLAYCs[SOILLAYCs$MATRIX=="Mineraal",  ]


attach(SOILLAYCs)
str(SOILLAYCs)
#View(SOILLAYCs)
#### Impute some missing values because SOILPROFILECOLLECTION object cannot live with NA's in topsoil horizons

SOILLAYCs$PLOTNAAM<-as.character(SOILLAYCs$PV)
SOILLAYCs$PROFID<-as.character(SOILLAYCs$PV)
SOILLAYCs$GEBIED<-as.character(SOILLAYCs$PV)
SOILLAYCs$TOP<-as.numeric(as.character(SOILLAYCs$VAN))
SOILLAYCs$BOT<-as.numeric(as.character(SOILLAYCs$TOT))
SOILLAYCs$CD<-as.numeric(as.character(SOILLAYCs$CD))
SOILLAYCs$GEM.DIEPTE<-as.numeric(as.character(SOILLAYCs$GEM.DIEPTE))


pv<-NULL
CsPROFmps<-NULL
plotlijst<-unique(SOILLAYCs$PROFID)
nplot<-length(plotlijst)

for (i in 1:nplot) {
#for (i in 1:5) {

  pv<-SOILLAYCs[SOILLAYCs$PROFID==plotlijst[i], ]

	# ordering/sorting matrix by depth or HSEQ
  pv<-pv[order(pv$GEM.DIEPTE), ] 
	
  # if CD are all NA => next
  if (is.na(min(unique(pv$CD)))) next #omit this plot when first Cd value is NA 
  
 	  PROF<-pv  	
		PROFID<-unique(PROF$PROFID)
		PLOTNAAM<-as.character(unique(PROF$PLOTNAAM))
		# set DEPTHSTOCK
		SOILDEPTH<-100
		### PROFSTOCK: apply SOILSPLINE function on PROF
	  out2<-Csmpsplinematrix(PROF)
		CDmatrix<-cbind(CDmatrix,out2)	
	next
		}
## end of routine

#############################
#### resulting datafiles ####
#############################

CDmatrix
View(CDmatrix)

## Toevoegen diepte kolom
str(CDmatrix)

CDmatrix[ ,1]<-1:100
names(CDmatrix)[1]<-"Diepte"



#### WRITE output

write.csv(CDmatrix,file="C:/DATA/Cmon3/AnalyseResultaten/Cmon_CD_profile_matrix.csv", row.names = FALSE)



#CDmatrix<-read.csv(file="C:/DATA/Cmon3/AnalyseResultaten/Cmon_CD_profile_matrix.csv")



##########################
par(mfrow=c(3,3))

View(CDmatrix)


maxCD<-max(CDmatrix[,2:10])
for (i in 2:10) {
plot(CDmatrix[,i],(-1*(1:100)), pch=16, main=names(CDmatrix)[i], 
              xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
lines(CDmatrix[,i],(-1*(1:100)), col="red", lwd=2)
}






### grouping by landuse
names(DF)
CmonLU<-distinct(DF[, c(2,62)])      ### hier juiste Cmon LU klasse

names(CmonLU)<-c("Bodemlocaties_Naam","Landgebruikscategorie.Cmon")

unique(CmonLU$Landgebruikscategorie.Cmon)
## remove NA
#CmonLU<-CmonLU[CmonLU$Landgebruikscategorie.Cmon!="nan",  ]
#dim(CmonLU)
#View(CmonLU)


### link with landuse
PVlijst<-names(CDmatrix)
unique(CmonLU$Landgebruikscategorie.Cmon)

head(CmonLU)
Akkerset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Akker" & substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Akkerset) 
length(Akkerset[substr(Akkerset,1,6)=="CMON_S"])  ## aantal schaduwplots

BGrasset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Blijvend grasland"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(BGrasset) 
length(BGrasset[substr(BGrasset,1,6)=="CMON_S"])  ## aantal schaduwplots

Bosset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Bos"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Bosset) 
length(Bosset[substr(Bosset,1,6)=="CMON_S"])  ## aantal schaduwplots

GrasbuitenLB<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Grasland buiten landbouw"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(GrasbuitenLB) 
length(GrasbuitenLB[substr(GrasbuitenLB,1,6)=="CMON_S"])  ## aantal schaduwplots


NietGrasOP<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Niet-grasland open natuur"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(NietGrasOP) 
length(NietGrasOP[substr(NietGrasOP,1,6)=="CMON_S"])  ## aantal schaduwplots


Bermenset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Ruimtebeslag bermen"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Bermenset) 
length(Bermenset[substr(Bermenset,1,6)=="CMON_S"])  ## aantal schaduwplots


Parkenset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Ruimtebeslag parken en recreatie"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Parkenset) 
length(Parkenset[substr(Parkenset,1,6)=="CMON_S"])  ## aantal schaduwplots


Tuinenset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Ruimtebeslag tuinen"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Tuinenset) 
length(Tuinenset[substr(Tuinenset,1,6)=="CMON_S"])  ## aantal schaduwplots

Overigeset<-CmonLU$Bodemlocaties_Naam[CmonLU$Landgebruikscategorie.Cmon=="Ruimtebeslag overige"& substr(CmonLU$Bodemlocaties_Naam,1,6)=="CMON_P"]
length(Overigeset) 
length(Overigeset[substr(Overigeset,1,6)=="CMON_S"])  ## aantal schaduwplots


### from dplyr:  select()   - combine all Akkers in CDmatrixAkker
nlijst<-names(CDmatrix)
"CMON_L57b2cdc"%in%Akkerset

CDmatrixAkker<-select(CDmatrix,all_of(Akkerset)); dim(CDmatrixAkker)
CDmatrixGras<-select(CDmatrix,all_of(BGrasset));dim(CDmatrixGras)
CDmatrixBos<-select(CDmatrix,all_of(Bosset));dim(CDmatrixBos)
CDmatrixGBLB<-select(CDmatrix,all_of(GrasbuitenLB)); dim(CDmatrixGBLB)
CDmatrixNGON<-select(CDmatrix,all_of(NietGrasOP)); dim(CDmatrixNGON)
CDmatrixBerm<-select(CDmatrix,all_of(Bermenset));dim(CDmatrixBerm)
CDmatrixPark<-select(CDmatrix,all_of(Parkenset));dim(CDmatrixPark)
CDmatrixTuinen<-select(CDmatrix,all_of(Tuinenset));dim(CDmatrixTuinen)
CDmatrixOverige<-select(CDmatrix,all_of(Overigeset));dim(CDmatrixOverige)


#### Akker   #############################################################
summary(CDmatrixAkker)

### plot vertical splines 
par(mfrow=c(3,3))
maxCD<-max(CDmatrixAkker)
nplots<-dim(CDmatrixAkker)[2]
for (i in 1:nplots) {
  plot(CDmatrixAkker[,i],(-1*(1:100)), pch=16, main=names(CDmatrixAkker)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixAkker[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
AkkerGem<-rowMeans(CDmatrixAkker,na.rm=T)

##########################################################################
#### Grasland  #############################################################
summary(CDmatrixGras)

### plot vertical splines Park
par(mfrow=c(3,3))
maxCD<-max(CDmatrixGras)
nplots<-dim(CDmatrixGras)[2]
for (i in 1:nplots) {
  plot(CDmatrixGras[,i],(-1*(1:100)), pch=16, main=names(CDmatrixGras)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixGras[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
GrasGem<-rowMeans(CDmatrixGras,na.rm=T)

##########################################################################
#### Bos  #############################################################
summary(CDmatrixBos)

### plot vertical splines Bos
par(mfrow=c(3,3))
maxCD<-max(CDmatrixBos)
nplots<-dim(CDmatrixBos)[2]
for (i in 1:nplots) {
  plot(CDmatrixBos[,i],(-1*(1:100)), pch=16, main=names(CDmatrixBos)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixBos[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
BosGem<-rowMeans(CDmatrixBos,na.rm=T)


##########################################################################
#### Gras buiten landbouw #############################################################
summary(CDmatrixGBLB)

### plot vertical splines GBLB
par(mfrow=c(3,3))
maxCD<-max(CDmatrixGBLB)
nplots<-dim(CDmatrixGBLB)[2]
for (i in 1:nplots) {
  plot(CDmatrixGBLB[,i],(-1*(1:100)), pch=16, main=names(CDmatrixGBLB)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixGBLB[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
GBLBGem<-rowMeans(CDmatrixGBLB,na.rm=T)

##########################################################################
#### GNiet Gras Open Natuur ##############################################
summary(CDmatrixNGON)

### plot vertical splines GBLB
par(mfrow=c(3,3))
maxCD<-max(CDmatrixNGON)
nplots<-dim(CDmatrixNGON)[2]
for (i in 1:nplots) {
  plot(CDmatrixNGON[,i],(-1*(1:100)), pch=16, main=names(CDmatrixNGON)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixNGON[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
NGONGem<-rowMeans(CDmatrixNGON,na.rm=T)



##########################################################################
#### Tuinen   #############################################################
summary(CDmatrixTuinen)

### plot vertical splines Park
par(mfrow=c(2,3))
maxCD<-max(CDmatrixTuinen)
nplots<-dim(CDmatrixTuinen)[2]
for (i in 1:nplots) {
  plot(CDmatrixTuinen[,i],(-1*(1:100)), pch=16, main=names(CDmatrixTuinen)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixTuinen[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
TuinenGem<-rowMeans(CDmatrixTuinen,na.rm=T)


##########################################################################
#### Park   #############################################################
summary(CDmatrixPark)

### plot vertical splines Park
par(mfrow=c(2,3))
maxCD<-max(CDmatrixPark)
nplots<-dim(CDmatrixPark)[2]
for (i in 1:nplots) {
  plot(CDmatrixPark[,i],(-1*(1:100)), pch=16, main=names(CDmatrixPark)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixPark[,i],(-1*(1:100)), col="red", lwd=2)
}

# average vertical spline
ParkGem<-rowMeans(CDmatrixPark,na.rm=T)


#### Berm   ####################################################

### plot vertical splines Berm
par(mfrow=c(2,3))
maxCD<-max(CDmatrixBerm)
nplots<-dim(CDmatrixBerm)[2]
for (i in 1:nplots) {
  plot(CDmatrixBerm[,i],(-1*(1:100)), pch=16, main=names(CDmatrixBerm)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixBerm[,i],(-1*(1:100)), col="red", lwd=2)
}

## average vertical spline
BermGem<-rowMeans(CDmatrixBerm,na.rm=T)



#### Overige  ####################################################

### plot vertical splines Berm
par(mfrow=c(2,3))
maxCD<-max(CDmatrixOverige)
nplots<-dim(CDmatrixOverige)[2]
for (i in 1:nplots) {
  plot(CDmatrixOverige[,i],(-1*(1:100)), pch=16, main=names(CDmatrixOverige)[i], 
       xlab=expression('Koolstof dichtheid   (t C ha'^-1*'cm'^-1*')'), ylab="Diepte (cm)", xlim=c(0,maxCD))
  lines(CDmatrixOverige[,i],(-1*(1:100)), col="red", lwd=2)
}

## average vertical spline
OverigeGem<-rowMeans(CDmatrixOverige,na.rm=T)



# cmon colour names
ColBos<-"#487524"
ColNatuur<-"#C04384"
ColBG<-"#A3CC00"
ColAkker<-"#F0D70F"
ColRB<-"#D26E25"
ColAfgedekt<-"#ED0731"
ColWater<-"#5BD9FF"
ColNietklas<-"#000000"
ColParken<-"#39B9BE"
ColTuinen<-"#644117"
ColRBOverige<-"#808080"
ColBermen<-"#15465B"
ColGrasbuitenlb<-"#FA7FBF"
ColNietgraslandON<-"#901994"

#####-----------------------------------------
#### plot gemiddelden per LU samen
par(mfrow=c(1,1))
par(xaxs="i", yaxs="i")    ### make X and Y axis to meet in 0
#plot(BermGem,(-1*(1:100)), pch="", main="Verticale distributie gemiddelde koolstofdichtheid per landgebruik", 
plot(BosGem,(-1*(1:100)), pch="", main="", 
          xlab=expression('Organische C dichtheid   (t C ha'^-1*'cm'^-1*')'), 
     ylab="Diepte onder maaiveld  (cm)", xlim=c(0,5),bty="n",xaxt="n",yaxt="n", cex.lab=1.3)
axis(3)
axis(2,las=1, at=(0:5)*-20, labels=c("0","20","40","60","80","100"))
lines(AkkerGem,(-1*(1:100)), col="#F0D70F", , lwd=4)
lines(GrasGem,(-1*(1:100)), col="#A3CC00", lwd=4)
lines(BosGem,(-1*(1:100)), col="#487524", lwd=4)
lines(GBLBGem,(-1*(1:100)), col="#FA7FBF", lwd=4)
lines(NGONGem,(-1*(1:100)), col="#901994", lwd=4)
lines(TuinenGem,(-1*(1:100)), col="#644117", lwd=4, lty=2)
lines(ParkGem,(-1*(1:100)), col="#39B9BE", lwd=4,lty=2)
lines(BermGem,(-1*(1:100)), col="#15465b", lwd=4,lty=2)
lines(OverigeGem,(-1*(1:100)), col="#808080", lwd=4, lty=2)

abline(h=-30, lty=3)

legend(2.5,-60,bty="n", c("Akker","Blijvend grasland","Bos","Grasland buiten landbouw","Niet-grasland open natuur","Ruimtebeslag tuinen","Ruimtebeslag parken en recreatie","Ruimtebeslag bermen","Ruimtebeslag overige"), 
       col=c("#F0D70F","#A3CC00","#487524","#FA7FBF","#901994","#644117","#39B9BE","#15465b","#808080"), lwd=4, cex=1.4,
       lty=c(1,1,1,1,1,2,2,2,2))


# only main categories NIET AANGEPAST voro 2025 !!!

RuimtebeslagGem<-rowMeans(cbind(TuinenGem,ParkGem,BermGem,OverigeGem),na.rm=T)
#####-----------------------------------------
#### plot gemiddelden per LU samen
par(mfrow=c(1,1))
par(xaxs="i", yaxs="i")    ### make X and Y axis to meet in 0
#plot(BermGem,(-1*(1:100)), pch="", main="Verticale distributie gemiddelde koolstofdichtheid per landgebruik", 
plot(BosGem,(-1*(1:100)), pch="", main="", 
     xlab=expression('Organische C dichtheid   (t C ha'^-1*'cm'^-1*')'), 
     ylab="Diepte onder maaiveld  (cm)", xlim=c(0,5),bty="n",xaxt="n",yaxt="n", cex.lab=1.3)
axis(3)
axis(2,las=1, at=(0:5)*-20, labels=c("0","20","40","60","80","100"))
lines(AkkerGem,(-1*(1:100)), col="#F0D70F", , lwd=4)
lines(GrasGem,(-1*(1:100)), col="#A3CC00", lwd=4)
lines(BosGem,(-1*(1:100)), col="#487524", lwd=4)
lines(NatuurGem,(-1*(1:100)), col="#C04384", lwd=4)
lines(RuimtebeslagGem,(-1*(1:100)), col="orange", lwd=4)


abline(h=-30, lty=3)

legend(2.5,-60,bty="n", c("Akker","Blijvend grasland","Bos","Natuur","Ruimtebeslag"), 
       col=c("#F0D70F","#A3CC00","#487524","#C04384","orange"), lwd=4, cex=1.4,
       lty=c(1,1,1,1,1))




#########################################################################################
##### separate splines per LU class
##### bootstrapped mean + CI95

summary(CDmatrixAkker)


# Function for bootstrapped mean and CI95
BootMeanCI95<-function(xvar)  {
### bootstrapping
theta <- function(x){mean(x,na.rm=T)}   # make function
# Determine CI95 from bootstrap B=1000
BCa<-bcanon(xvar, 1000, theta, alpha=c(0.025,0.975))
GEM<-mean(BCa$u, na.rm=T)   ## mean value all bootstrapped distributions
CI95<-BCa$confpoints  ## 95 confidence
CI95Lo<-CI95[1,2]
CI95Hi<-CI95[2,2]
OUT<-c(GEM,CI95Lo,CI95Hi)
names(OUT)<-c("Mean","CI95Lo","CI95Hi")
return (OUT)
}


##### Compute mean and CI95 for CD matrix for all depths
str(CDmatrixAkker)
#
CDMatrix<-CDmatrixAkker
BOOTAKKEROUT<-NULL

for (i in 1:100)   {
  xx<-as.numeric(CDMatrix[i, ]) ### select all values of row i  (depth i)
  str(xx)
  out<-BootMeanCI95(xx)   #bootstrap all values of depth i
  BOOTAKKEROUT<-rbind(BOOTAKKEROUT,out)
next
}
BOOTAKKEROUT<-data.frame(BOOTAKKEROUT, row.names = NULL)

## smoothen the Lo and Hi confidence intervals
Depth<-(-1*c(1:100))
LL<-approx(BOOTAKKEROUT$CI95Lo,Depth, method ="linear")
UL<-approx(BOOTAKKEROUT$CI95Hi,Depth, method ="linear")
BOOTAKKEROUT$LL.x<-rev(LL$x)  # smoothened LL x value
BOOTAKKEROUT$LL.y<-rev(LL$y)
BOOTAKKEROUT$UL.x<-rev(UL$x)
BOOTAKKEROUT$UL.y<-rev(UL$y)
BOOTAKKEROUT



##### plot this mean with 95% interval zone
plot(BOOTAKKEROUT$Mean,(-1*(1:100)), col="yellow", type="l", lwd=3, lty=1, xlim=c(0,6), main=c("Carbon density Akker versus Bos"))
#lines(BOOTAKKEROUT$CI95Lo,(-1*(1:100)), col="yellow", lwd=3, lty=3)
#lines(BOOTAKKEROUT$CI95Hi,(-1*(1:100)), col="yellow", lwd=3, lty=3)
#lines(approx(BOOTAKKEROUT$CI95Lo,(-1*(1:100))), col="orange", lwd=2, lty=1)
#lines(approx(BOOTAKKEROUT$CI95Hi,(-1*(1:100))), col="blue", lwd=2, lty=1)

### 95% confint zone
x<-BOOTAKKEROUT$LL.x
ix <- sort(x,index.return=T)$ix
polygon(c(rev(BOOTAKKEROUT[ix,6]), BOOTAKKEROUT[ix,4]),c(rev(BOOTAKKEROUT[ix,7]),BOOTAKKEROUT[ix,5]),col = rgb(1,1,0,0.1), border = NA)  



##### BOS
##### Compute mean and CI95 for CD matrix for all depths
str(CDmatrixBos)
#
CDMatrix<-CDmatrixBos
BOOTBOSOUT<-NULL

for (i in 1:100)   {
  xx<-as.numeric(CDMatrix[i, ]) ### select all values of row i  (depth i)
  str(xx)
  out<-BootMeanCI95(xx)   #bootstrap all values of depth i
  BOOTBOSOUT<-rbind(BOOTBOSOUT,out)
  next
}

BOOTBOSOUT<-data.frame(BOOTBOSOUT, row.names = NULL)


## smoothen the Lo and Hi confidence intervals
LL<-approx(BOOTBOSOUT$CI95Lo,Depth, method ="linear",na.rm=T)
UL<-approx(BOOTBOSOUT$CI95Hi,Depth, method ="linear",na.rm=T)
BOOTBOSOUT$LL.x<-rev(LL$x)  # smoothened LL x value
BOOTBOSOUT$LL.y<-rev(LL$y)
BOOTBOSOUT$UL.x<-rev(UL$x)
BOOTBOSOUT$UL.y<-rev(UL$y)
BOOTBOSOUT



##### plot this mean with 95% interval zone
##### plot this mean with 95% interval zone

lines(BOOTBOSOUT$Mean,(-1*(1:100)), col="#487524", type="l", lwd=3, lty=1)
#lines(BOOTBOSOUT$CI95Lo,(-1*(1:100)), col="yellow", lwd=3, lty=3)
lines(BOOTBOSOUT$CI95Hi,(-1*(1:100)), col="yellow", lwd=3, lty=3)
lines(approx(BOOTBOSOUT$CI95Lo,(-1*(1:100))), col="orange", lwd=2, lty=1)
lines(approx(BOOTBOSOUT$CI95Hi,(-1*(1:100))), col="blue", lwd=2, lty=1)

### 95% confint zone
x<-BOOTBOSOUT$LL.x
ix <- sort(x,index.return=T)$ix
polygon(c(rev(BOOTBOSOUT[ix,6]), BOOTBOSOUT[ix,4]),c(rev(BOOTBOSOUT[ix,7]),BOOTBOSOUT[ix,5]),col = rgb(0,1,0,0.1), border = NA)  

abline(h=-30, lty=2, col="grey", lwd=2)


########################

### make a figure matrix with Cropland as row, and grassland, forest, ... as columns
### second row grassland ...



##### MAKE functions: single and double splines

CDmat1<-CDmatrixBos
colcode1<-"#487524"
msd<-100  # max soil depth in cm

#### input CDmatrix (1-100 cm diepte), colour code
CDMatrixOneSplineCI95<-function(CDmat1, msd, colcode1)   {
  
  CDMatrix<-CDmat1
  BOOTOUT<-NULL
  Depth<-(-1*(1:msd))
  
  for (i in 1:msd)   {
    xx<-as.numeric(CDMatrix[i, ]) ### select all values of row i  (depth i)
    str(xx)
    out<-BootMeanCI95(xx)   #bootstrap all values of depth i
    BOOTOUT<-rbind(BOOTOUT,out)
    next
  }
  
  BOOTOUT<-data.frame(BOOTOUT, row.names = NULL)
  
  
  
  ## smoothen the Lo and Hi confidence intervals
  LL<-approx(BOOTOUT$CI95Lo,Depth, method ="linear",na.rm=T)
  UL<-approx(BOOTOUT$CI95Hi,Depth, method ="linear",na.rm=T)
  BOOTOUT$LL.x<-rev(LL$x)  # smoothened LL x value
  BOOTOUT$LL.y<-rev(LL$y)
  BOOTOUT$UL.x<-rev(UL$x)
  BOOTOUT$UL.y<-rev(UL$y)
  BOOTOUT
  
  max.x<-max(BOOTOUT$UL.x) 
  ##### plot this mean with 95% interval zone
  plot(BOOTOUT$Mean,(-1*(1:msd)), col=colcode1, type="l", lwd=3,xlim=c(0,6), main=c("Carbon density"),
       xlab=expression('Organische C dichtheid   (t C ha'^-1*'cm'^-1*')'), 
       ylab="Diepte onder maaiveld  (cm)")
  #lines(BOOTBOSOUT$CI95Lo,(-1*(1:100)), col="yellow", lwd=3, lty=3)
  lines(approx(BOOTOUT$CI95Lo,(-1*(1:msd))),xout=BOOTOUT$LL.x,  col="grey", lwd=2, lty=1)
  lines(approx(BOOTOUT$CI95Hi,(-1*(1:msd))),xout=BOOTOUT$UL.y,  col="grey", lwd=2, lty=1)
  
  ### 95% confint zone
  x<-BOOTOUT$LL.x
  ix <- sort(x,index.return=T)$ix
  zonecol<-paste0(colcode1,"A1")
  polygon(c(rev(BOOTOUT[ix,6]), BOOTOUT[ix,4]),c(rev(BOOTOUT[ix,7]),BOOTOUT[ix,5]),col = zonecol, border = NA)  
  
  abline(h=-30, lty=2, col="grey", lwd=2)
  
  return(BOOTOUT)
  } 
### end of function


par(mfrow=c(1,1))
CDmat1<-CDmatrixPark
colcode1<-"#487524"
msd<-100  # max soil depth in cm


CDMatrixOneSplineCI95(CDmatrixBos,100, "#487524")

XXX<-CDMatrixOneSplineCI95(CDmatrixAkker,100, "#F0D70F")







### TABEL IIN RAPPORT



round(summary(AkkerGem),2)
sd(AkkerGem)
100*(sd(AkkerGem)/mean(AkkerGem))

round(summary(GrasGem),2)
sd(GrasGem)
100*(sd(GrasGem)/mean(GrasGem))

round(summary(BosGem),2)
sd(BosGem)
100*(sd(BosGem)/mean(BosGem))

round(summary(GBLBGem),2)
sd(GBLBGem)
100*(sd(GBLBGem)/mean(GBLBGem))

round(summary(NGONGem),2)
sd(NGONGem)
100*(sd(NGONGem)/mean(NGONGem))

round(summary(TuinenGem),2)
sd(TuinenGem)
100*(sd(TuinenGem)/mean(TuinenGem))

round(summary(ParkGem),2)
sd(ParkGem)
100*(sd(ParkGem)/mean(ParkGem))

round(summary(BermGem),2)
sd(BermGem)
100*(sd(BermGem)/mean(BermGem))

round(summary(OverigeGem),2)
sd(OverigeGem)
100*(sd(OverigeGem)/mean(OverigeGem))


