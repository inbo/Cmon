###########################--
## Get Cmon plotnumber
## Bruno De Vos 04/11/2023
##########################--

# Init ----
library(raster)
setwd("C:/R_scripts/_GIT_REPO/Cmon/data/")

# 1. Load 10x10m raster for georeference ----  
# n= 215513424 grid cells
# extent LB72 metric: 22000, 258880, 153050, 244030  (xmin, xmax, ymin, ymax)
# values are 2016 landuse c("1.Forest","2.Nature","3.Grassland","4.Cropland",
# "5.Residential","6.Sealed","7.Water","8.Unclassified")
r.LUCMon<-raster("./Cmon_base_raster_10x10.tif")  ###  in trimmed version
# optional: plot(r.LUCMon)

# 2. generic function getplotnr with LB72 XY coords as input ----
getplotnr<-function(LB72X,LB72Y) {
coordlist<-cbind(LB72X,LB72Y)  
### Determine cell in raster for x,y
PLOTNR<-cellFromXY(r.LUCMon,coordlist)
return(PLOTNR)
}


# example: getplotnr(128112,156835)


