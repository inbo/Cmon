### Standaard GRTS sample scheme 
### Trekking van 50 locaties per plot
### Subset 50 locaties voor Survey 1 + Schaduwmeetnet incl reservelocaties
### Mogelijks zelfde configuratie alle proefvlakken
### Bruno De Vos - Code programmed 2/12/2021
### Aangepast met header stripper Trimble GPS files op 22/12/2022


library(grtsdb)
library(tidyverse)
library(sf)
library(readxl)



## set working directory for outputs
setwd("C:/R/out/DVW/Lijst/")
path<-"C:/R/out/DVW/Lijst/"

db <- connect_db()
#db <- connect_db("standardconfig.sqlite")  # create specific sampling scheme 


## read list of plots from Excel file
## readxl
LIJST<-read_excel(path="C:/R_scripts/R_GRTS/DATA/Centrumcoördinaten_staalname_locaties_C_stocks_DVW_valleigebieden_2.xlsx")
head(LIJST)
nLijst<-length(LIJST$CODE)

#View(LIJST)

#### FUNCTION 

GRTSSCHEMA<-function(ID,LBX,LBY) {

## Hoekpunten  begin rechtsonder - clockwise

LB72x.H1<-LB72x.C-5
LB72y.H1<-LB72y.C-5

LB72x.H2<-LB72x.C-5
LB72y.H2<-LB72y.C+5

LB72x.H3<-LB72x.C+5
LB72y.H3<-LB72y.C+5

LB72x.H4<-LB72x.C+5
LB72y.H4<-LB72y.C-5

# bounding box van het proefvlak in Lambert72 coordinaten
# bounding box van xmin tot xmax, ymin tot ymax
bb <- rbind(
  x = c(LB72x.H1, LB72x.H4),
  y = c(LB72y.H1, LB72y.H2)
)

## add level to sqlite db and select 50 samples
add_level(bbox = bb, cellsize = 0.01, grtsdb = db)
full_grid <- extract_sample(
  grtsdb = db, samplesize = 50, bbox = bb, cellsize = 0.01
)

full_grid %>%
  arrange(x1c, x2c) %>%
  pivot_wider(names_from = "x2c", values_from = "ranking") %>%
  select(-x1c) %>%
  as.matrix()

## Create sample list 

SLIST<-full_grid
names(SLIST)<-c("LB72x","LB72y","Rank")

LOC<-1:50   ## better to start from 1
SLIST<-cbind(LOC,SLIST)
str(SLIST)


### PLOT file
Subtitle<-paste0("Plot Center: LB72x = ",LB72x.C," LB72y = ",LB72y.C)

plot(SLIST$LB72x,SLIST$LB72y, pch=16, col="grey", xlab="LB72x (m)", ylab="LB72y (m)",
     xlim=c(LB72x.H1-0.5,LB72x.H4+0.5),ylim=c(LB72y.H1-0.5,LB72y.H2+0.5))
title(PLOTID, sub = Subtitle, 
      cex.main = 1.5, col.main= "black",
      cex.sub = 0.75, col.sub = "blue")


## Locations
text(SLIST$LB72x,SLIST$LB72y+0.2,SLIST$LOC, col="dark grey", cex=1.2)

## Plot subset 
SUBLIST1<-SLIST[SLIST$LOC<=7,]   # set to sample deep
SUBLIST2<-SLIST[SLIST$LOC>7&SLIST$LOC<=16,]   # standard sampling

# display subsets in colour
points(SUBLIST1$LB72x,SUBLIST1$LB72y, col="blue", pch=16)
text(SUBLIST1$LB72x,SUBLIST1$LB72y+0.2,SUBLIST1$LOC, col="blue", cex=1.2)

for (i in 1:7) {
segments(LB72x.C,LB72y.C,SUBLIST1$LB72x[i],SUBLIST1$LB72y[i],lty=2, lwd=1, col="blue")
}

# display subsets in colour
points(SUBLIST2$LB72x,SUBLIST2$LB72y, col="green", pch=16)
text(SUBLIST2$LB72x,SUBLIST2$LB72y+0.2,SUBLIST2$LOC, col="green", cex=1.2)

for (j in 1:9) {
  segments(LB72x.C,LB72y.C,SUBLIST2$LB72x[j],SUBLIST2$LB72y[j],lty=2, lwd=1, col="green")
} 


#plot center
points(LB72x.C,LB72y.C, col="blue", pch=13, cex=2)   # centrum

#plot corners
text(LB72x.H1,LB72y.H1, "H1", col="blue", cex=2)
text(LB72x.H2,LB72y.H2, "H2", col="blue", cex=2)
text(LB72x.H3,LB72y.H3, "H3", col="blue", cex=2)
text(LB72x.H4,LB72y.H4, "H4", col="blue", cex=2)
segments(LB72x.H1,LB72y.H1,LB72x.H2,LB72y.H2,lty=3, lwd=1, col="blue")
segments(LB72x.H2,LB72y.H2,LB72x.H3,LB72y.H3,lty=3, lwd=1, col="blue")
segments(LB72x.H3,LB72y.H3,LB72x.H4,LB72y.H4,lty=3, lwd=1, col="blue")
segments(LB72x.H4,LB72y.H4,LB72x.H1,LB72y.H1,lty=3, lwd=1, col="blue")

### Make CSV list

header1<-c("C",LB72x.C,LB72y.C,0)
header2<-c("H1",LB72x.H1,LB72y.H1,0)
header3<-c("H2",LB72x.H2,LB72y.H2,0)
header4<-c("H3",LB72x.H3,LB72y.H3,0)
header5<-c("H4",LB72x.H4,LB72y.H4,0)

header<-data.frame(rbind(header1,header2,header3,header4,header5))
names(header)<-c("LOC","LB72x","LB72y","Rank")

### Combine Header & pointslist

COORDSLIST<-rbind(header,SLIST)
names(COORDSLIST)
str(COORDSLIST)

# make sure numeric
COORDSLIST$LB72x<-as.numeric(COORDSLIST$LB72x)
COORDSLIST$LB72y<-as.numeric(COORDSLIST$LB72y)
COORDSLIST$Rank<-as.numeric(COORDSLIST$Rank)

# coordinaten van Lambert72 naar WGS84
WGS84.DD<-st_as_sf(COORDSLIST, coords = c("LB72x", "LB72y"), crs = 31370) %>%
  st_transform(crs = 4326) %>%
  st_coordinates()
WGS84<-data.frame(WGS84.DD)
str(WGS84)

OUTLIST<-cbind(COORDSLIST,WGS84)
names(OUTLIST)<-c("LOC","LB72x","LB72y","Rank","WGS84LONG","WGS84LAT")

OUTLIST

### write output file

fname<-paste0("Coord_",PLOTID,".csv")
outpath<-paste0(path,fname)
write.csv(OUTLIST,outpath,row.names = F)

## Trimble GPS file
names(OUTLIST)

PLS<-rep(PLOTID,times=length(OUTLIST$LOC))
Trimble<-data.frame(OUTLIST[,c(1:3)],PLS)
str(Trimble)


fname<-paste0("Trimble_",PLOTID,".csv")
outpath<-paste0(path,fname)
write.csv(Trimble,outpath,row.names = F, col.names = NULL)


### Plot graph

fname<-paste0("Graph_",PLOTID,".jpg")
plotfilenaam<-paste0(path,fname)  
jpeg(filename = plotfilenaam, type=c("windows"),width = 750, height = 750, quality=100)   ### create file to save plot


Subtitle<-paste0("Plot Center: LB72x = ",LB72x.C," LB72y = ",LB72y.C)


plot(SLIST$LB72x,SLIST$LB72y, pch=16, col="grey", xlab="LB72x (m)", ylab="LB72y (m)",
     xlim=c(LB72x.H1-0.5,LB72x.H4+0.5),ylim=c(LB72y.H1-0.5,LB72y.H2+0.5))
title(PLOTID, sub = Subtitle, 
      cex.main = 1.5, col.main= "black",
      cex.sub = 0.75, col.sub = "blue")

## Locations
text(SLIST$LB72x,SLIST$LB72y+0.2,SLIST$LOC, col="dark grey", cex=1.2)

## Plot subset 
SUBLIST1<-SLIST[SLIST$LOC<=7,]   # set to sample deep
SUBLIST2<-SLIST[SLIST$LOC>7&SLIST$LOC<=16,]   # standard sampling

# display subsets in colour
points(SUBLIST1$LB72x,SUBLIST1$LB72y, col="blue", pch=16)
text(SUBLIST1$LB72x,SUBLIST1$LB72y+0.2,SUBLIST1$LOC, col="blue", cex=1.2)

for (i in 1:7)  {
  segments(LB72x.C,LB72y.C,SUBLIST1$LB72x[i],SUBLIST1$LB72y[i],lty=2, lwd=1, col="blue")
}

# display subsets in colour
points(SUBLIST2$LB72x,SUBLIST2$LB72y, col="green", pch=16)
text(SUBLIST2$LB72x,SUBLIST2$LB72y+0.2,SUBLIST2$LOC, col="green", cex=1.2)

for (j in 1:9) {
  segments(LB72x.C,LB72y.C,SUBLIST2$LB72x[j],SUBLIST2$LB72y[j],lty=2, lwd=1, col="green")
}

#plot center
points(LB72x.C,LB72y.C, col="blue", pch=13, cex=2)   # centrum

#plot corners
text(LB72x.H1,LB72y.H1, "H1", col="blue", cex=2)
text(LB72x.H2,LB72y.H2, "H2", col="blue", cex=2)
text(LB72x.H3,LB72y.H3, "H3", col="blue", cex=2)
text(LB72x.H4,LB72y.H4, "H4", col="blue", cex=2)
segments(LB72x.H1,LB72y.H1,LB72x.H2,LB72y.H2,lty=3, lwd=1, col="blue")
segments(LB72x.H2,LB72y.H2,LB72x.H3,LB72y.H3,lty=3, lwd=1, col="blue")
segments(LB72x.H3,LB72y.H3,LB72x.H4,LB72y.H4,lty=3, lwd=1, col="blue")
segments(LB72x.H4,LB72y.H4,LB72x.H1,LB72y.H1,lty=3, lwd=1, col="blue")



dev.off()   #  save jpg plot

}  # End function
###################


names(LIJST)
head(LIJST)

## LOOP for all plots

for (ii in 1:nLijst) {
  #for (ii in 1:5) {  
  ## centrum coordinaat plot (uit lijst te halen)
  PLOTID<-LIJST$CODE[ii]
  LB72x.C<-LIJST$POINT_X[ii]
  LB72y.C<-LIJST$POINT_Y[ii]
  
  GRTSSCHEMA(PLOTID,LB72x.C,LB72y.C)
}



##################################################
##### Omit header line from TRIMBLE GPS CSV files

### set working directory for outputs
setwd("C:/R/out/DVW/Lijst/")


### make trimble list
TRLIJST<-list.files(getwd(), pattern=c("Trimble_"))
nTRLIJST<-length(TRLIJST)


for (i in 1:nTRLIJST)  {
  
  TMP<-read.csv(file = TRLIJST[i])
  write.table(TMP, file=TRLIJST[i], sep=",",  col.names=FALSE, row.names=FALSE)
  
  print(i)
}



## add Cmon PLOTID field
names(LIJST)
LIJST$PLOTID<-getplotid(LIJST$POINT_X,LIJST$POINT_Y)
LIJST


