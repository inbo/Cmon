#### READ Json file 
#### Bruno de Vos 1/12/2023
(.packages())  ## packages running

library(jsonlite)

setwd(dir = "C:/R/OUT/LDTEX/2022/INBO/json/")
JSONfile<-"CMON_P1b64771_laag_0_10_MG_2022_2023-04-21.json"


TEXINBO<-fromJSON(JSONfile)

View(TEXINBO)
str(TEXINBO)


TEXINBO.json<-toJSON(TEXINBO, pretty = TRUE)

jsonlite::fromJSON(TEXINBO.json)   ### according to jasonlite package



### prettify a json string, but 
prettify(TEXINBO.json)
minify(TEXINBO.json)


#write(TEXINBO.json, "C:/R_scripts/R_C-Mon/R_Json_omzettingen/Textuurfiles/TextINBO.json")                  







