#### Converting a hex-based PLOTID to a Cmon PLOTNR
#### Bruno De Vos  - Nov 2023
# PLOTNRs are unique 10x10 m CMON gridcel numbers for Flanders Region 
# e.g. PLOTID = "P5e53833"

#PLOTID TO PLOTNR
plotid2plotnr<-function(PLOTID) {
  PLOTID<-as.character(PLOTID)
  if (substring(PLOTID,1,1)=="P")
    hexnr<-substring(PLOTID,2,nchar(PLOTID))
  PLOTNR<-as.integer(as.hexmode(hexnr))
  return(PLOTNR)              
}

# execute code

# plotid2plotnr("P5e53833")
