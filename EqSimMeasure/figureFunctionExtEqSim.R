# I made a separate function for the external data because I only use the SDF results from those data
# The separate function is shorter, then.

figureFunctionExtEqSim<-function(n.use, path.stem) {

# same for all characteristics
fname.reg.EQ1<-paste(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,"Ext.RData", sep="") # EQ 1-sided
fname.reg.EQ2<-paste(path.stem,"resultsEQ1sideDeltaPonly0.2maxalpha1n",n.use,"Ext.RData", sep="") # EQ 2-sided
fname.reg.Ord1<-paste(path.stem,"resultsSOWeibull1siden",n.use,"Ext.RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"resultsSOWeibull2siden",n.use,"Ext.RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# EQ 1sided
load(fname.reg.EQ1)
results.SDF.EQ1side<-results.SDF.equiv   # SD without dropping

# make room for EQ2 (for good measure)
rm(results.SDF.equiv)


# EQ 2-sided
load(fname.reg.EQ2)
results.SDF.EQ2side<-results.SDF.equiv   # SD  without dropping

rm(results.SDF.equiv)


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
       results.SDF.EQ2side, 
       results.SDF.EQ1side, 
       results.SDF.SO1side, 
       results.SDF.SO2side
     )
     
     )

}
