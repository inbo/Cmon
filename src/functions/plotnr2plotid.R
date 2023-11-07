#### Converting a PLOTNR to a hex-based PLOTID
#### Bruno De Vos  - Nov 2023
# PLOTNRs are unique 10x10 m CMON gridcel numbers for Flander Region 
# e.g. PLOTNR=98908211

plotnr2plotid<-function(PLOTNR) {paste0("P",as.character(as.hexmode(PLOTNR)))}

# plotnr2plotid(PLOTNR)