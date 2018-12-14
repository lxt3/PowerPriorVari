
figureFunctionEqSim<-function(n.use, path.stem) {

# With Equivalence Similarity measure, we have EQ 1 and 2-sided, 
# as well as PP (posterior prob) 1 and 2-sided
  
  
# same for all characteristics
fname.reg.EQ1<-paste(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,".RData", sep="") # EQ 1-sided
fname.reg.EQ2<-paste(path.stem,"resultsEQ2sideDelta0.2maxalpha1n",n.use,".RData", sep="") # EQ 2-sided
fname.reg.Ord1<-paste(path.stem,"resultsSOWeibull1siden",n.use,".RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"resultsSOWeibull2siden",n.use,".RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# EQ 1sided
load(fname.reg.EQ1)
results.alpha.EQ1side<-results.alpha.equiv  # alpha0
results.prob.dropF.EQ1side<-results.prob.dropF.equiv  # type I error without dropping
results.prob.dropT.EQ1side<-results.prob.dropT.equiv  # type I error with dropping
results.biasF.EQ1side<-results.biasF.equiv   # bias results without dropping
results.biasT.EQ1side<-results.biasT.equiv   # bias results with dropping
results.SDF.EQ1side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ1side<-results.SDT.equiv   # SD results with dropping


# make room for EQ2 (for good measure)
rm(results.prob.dropF.equiv, results.prob.dropT.equiv,results.alpha.equiv, results.biasF.equiv, results.biasT.equiv,
   results.SDF.equiv, results.SDT.equiv)


# EQ 2-sided
load(fname.reg.EQ2)
results.alpha.EQ2side<-results.alpha.equiv
results.prob.dropF.EQ2side<-results.prob.dropF.equiv  # type I error without dropping
results.prob.dropT.EQ2side<-results.prob.dropT.equiv
results.biasF.EQ2side<-results.biasF.equiv   # bias results without dropping
results.biasT.EQ2side<-results.biasT.equiv   # bias results with dropping
results.SDF.EQ2side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ2side<-results.SDT.equiv   # SD results with dropping

rm(results.prob.dropF.equiv, results.prob.dropT.equiv,results.alpha.equiv, results.biasF.equiv, results.biasT.equiv,
   results.SDF.equiv, results.SDT.equiv)


# SO 1-sided
load(fname.reg.Ord1)
results.alpha.SO1side<-results.alpha.wbord  # alpha0
results.prob.dropF.SO1side<-results.prob.dropF.wbord  # type I error without dropping
results.prob.dropT.SO1side<-results.prob.dropT.wbord 
results.biasF.SO1side<-results.biasF.wbord   # bias without dropping
results.biasT.SO1side<-results.biasT.wbord
results.SDF.SO1side<-results.SDF.wbord   # SD without dropping
results.SDT.SO1side<-results.SDT.wbord

# make room for SO2
rm(results.alpha.wbord,results.prob.dropF.wbord,results.prob.dropT.wbord, results.biasF.wbord, 
   results.biasT.wbord, results.SDF.wbord, results.SDT.wbord)


# SO 2-sided
load(fname.reg.Ord2)
results.alpha.SO2side<-results.alpha.wbord  # alpha0
results.prob.dropF.SO2side<-results.prob.dropF.wbord  # type I error without dropping
results.prob.dropT.SO2side<-results.prob.dropT.wbord
results.biasF.SO2side<-results.biasF.wbord   # bias without dropping
results.biasT.SO2side<-results.biasT.wbord
results.SDF.SO2side<-results.SDF.wbord   # SD without dropping
results.SDT.SO2side<-results.SDT.wbord


rm(results.alpha.wbord,results.prob.dropF.wbord,results.prob.dropT.wbord, results.biasF.wbord, 
   results.biasT.wbord, results.SDF.wbord, results.SDT.wbord)


# gather the vectors for output
list(alpha=c(results.alpha.EQ2side, 
        results.alpha.EQ1side,
        results.alpha.SO1side,
        results.alpha.SO2side), 
     
     typeI=c(results.prob.dropF.EQ2side[1:20], results.prob.dropT.EQ2side[1:20],
     results.prob.dropF.EQ1side[1:20], results.prob.dropT.EQ1side[1:20],
     results.prob.dropF.SO1side[1:20],results.prob.dropT.SO1side[1:20],
     results.prob.dropF.SO2side[1:20], results.prob.dropT.SO2side[1:20]),
     
     bias=c(
       results.biasF.EQ2side, results.biasT.EQ2side,
       results.biasF.EQ1side, results.biasT.EQ1side,
       results.biasF.SO1side, results.biasT.SO1side,
       results.biasF.SO2side, results.biasT.SO2side
     ),
     
     sdF=c(
       results.SDF.EQ2side[1:20],
       results.SDF.EQ1side[1:20],
       results.SDF.SO1side[1:20],
       results.SDF.SO2side[1:20]
     )
     
     )

}
