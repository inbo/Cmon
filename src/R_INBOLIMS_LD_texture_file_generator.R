## LD Texture_processor COULTER files from LAB
## Programmed by Pieter Verschelde 9/06/2022
## adapted by Bruno De Vos

### !!! Be sure to have VPN connection to link to LIMS system !!!



library(dplyr)
library(jsonlite)

# Download/update met laatste versie 

#remotes::install_github('inbo/inbolims')


getwd() #gewoon om te tonen in welke werkdirectory je zit

library(tidyverse) #package met veel datafunctionaliteit
library(inbolims) #package die de verwerking van de textuurfiles regelt
library(DBI) #package voor DB communicatie
library(readxl)

###  JSON conversion function


TEX_CSV2JSON<-function(fullfilename)   {
  # read file
  TEXTUUR.CSV<-read.csv2(fullfilename)
  # making a list for json data export
  # names(TEXTUUR.CSV)
  sid<-unique(TEXTUUR.CSV$FieldSampleID)
  labsamplecode<-unique(TEXTUUR.CSV$sample)
  metingen<-TEXTUUR.CSV[,c(3:6)]
  
  TEXTUUR.list<-list(sid=sid,labsamplecode=labsamplecode,metingen=metingen)
  
  # convert to json in compact format
  #  TEXTUUR.json.pretty<-toJSON(TEXTUUR.list, pretty=TRUE)
  TEXTUUR.json<-toJSON(TEXTUUR.list, pretty=FALSE)  ### preferred format for INBOdem readin
  TEXTUUR.json
  # write output in same folder but with json extension
  fullfilename
  basefilename<-gsub("\\.csv$","",fullfilename)   ## remove extension
  jsonfilename<-paste0(basefilename,".json")
  
  write(TEXTUUR.json, jsonfilename)
  return(TEXTUUR.json)
}


#############################
## CENTRAL LOOP




#filename<-"C:/R/IN/LDTEX/Export_textuur_voorbeeld.txt"

## load raw filenames in folder  "C:/R/IN/LDTEX/" voor labproject V-22V057 (Cmon)
listFN<-list.files(path="C:/R/IN/LDTEX/2022/INBO/", pattern="V-22V057", full.names = TRUE)
nlistFN<-length(listFN)

## Loop to process all files serially   ####
for (i in 1:nlistFN) {

#for (i in 3:3) {
filename<-listFN[i]

#read.csv2(filename)
  
  #definieer de directory voor de geparste bestandjes
target_dir <- "C:/R/OUT/LDTEX/2022/INBO/"

#parse de file naar een geldige R dataset
textuur_parsed <- parse_texture_content(filename, delim = "\t")
View(textuur_parsed)

#interpreteer de dataset tot een inhoudelijk bruikbaar formaat
textuur_interpreted <- interprate_texture_content(textuur_parsed)
View(textuur_interpreted)

#maak een connectie met het LIMS datawarehouse
conn <- lims_connect() #connect to dwh
textuur_linked <- link_labo_id(conn, textuur_interpreted)
dim(textuur_linked)

#schrijf de files weg in /R/OUT/LDTEX/
write_texture_files(target_dir, textuur_linked)

}   # loop end




#### Process files and save to CSV and json ####


listFNOUT<-list.files(path="C:/R/OUT/LDTEX/2022/INBO/", pattern=".csv", full.names = TRUE)
nlist<-length(listFNOUT)


for (j in 1:nlist) {
TEMPfile<-read.csv2(listFNOUT[j]) 
dim(TEMPfile)
UNIfile<-distinct(TEMPfile)     ## remove all duplicate rows
dim(UNIfile)
# write output csv file
write.csv2(UNIfile,listFNOUT[j], row.names = FALSE)

# run function to convert to json and write in same directory
TEX_CSV2JSON(listFNOUT[j])
}
    
    






