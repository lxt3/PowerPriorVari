# I made a separate function for the external data because I only use the SDF results from those data
# The separate function is shorter, then.

figureFunctionExt<-function(n.use, path.stem) {

# same for all characteristics
fname.reg.KS1<-paste(path.stem,"resultsKSWeibull1siden",n.use,"Ext.RData", sep="") # KS 1-sided
fname.reg.KS2<-paste(path.stem,"resultsKSWeibull2siden",n.use,"Ext.RData", sep="") # KS 2-sided
fname.reg.Ord1<-paste(path.stem,"resultsSOWeibull1siden",n.use,"Ext.RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"resultsSOWeibull2siden",n.use,"Ext.RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# KS 1sided
load(fname.reg.KS1)
results.SDF.KS1side<-results.SDF.wb   # SD without dropping

# make room for KS2 (for good measure)
rm(results.SDF.wb)


# KS 2-sided
load(fname.reg.KS2)
results.SDF.KS2side<-results.SDF.wb   # SD  without dropping

rm(results.SDF.wb)


# SO 1-sided
load(fname.reg.Ord1)
results.SDF.SO1side<-results.SDF.wbord   # SD without dropping

# make room for SO2
rm(results.SDF.wbord)


# SO 2-sided
load(fname.reg.Ord2)
results.SDF.SO2side<-results.SDF.wbord   # SD without dropping

rm(results.SDF.wbord)


# gather the vectors for output
list(sdF=c(
       results.SDF.KS2side, 
       results.SDF.KS1side, 
       results.SDF.SO1side, 
       results.SDF.SO2side
     )
     
     )

}
