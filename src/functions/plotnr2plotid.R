<<<<<<< HEAD
#### Converting a PLOTNR to a hex-based PLOTID
#### Bruno De Vos  - Nov 2023
# PLOTNRs are unique 10x10 m CMON gridcel numbers for Flander Region 
# e.g. PLOTNR=98908211

plotnr2plotid<-function(PLOTNR) {paste0("P",as.character(as.hexmode(PLOTNR)))}

=======
#### Converting a PLOTNR to a hex-based PLOTID
#### Bruno De Vos  - Nov 2023
# PLOTNRs are unique 10x10 m CMON gridcel numbers for Flander Region 
# e.g. PLOTNR=98908211

plotnr2plotid<-function(PLOTNR) {paste0("P",as.character(as.hexmode(PLOTNR)))}

>>>>>>> ace476ac6c9956ed2c1cfe3dadb7cd4153f9e61a
# plotnr2plotid(PLOTNR)