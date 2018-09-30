
figureFunction<-function(n.use, path.stem) {

# same for all characteristics
fname.reg.KS1<-paste(path.stem,"resultsKSWeibull1siden",n.use,".RData", sep="") # KS 1-sided
fname.reg.KS2<-paste(path.stem,"resultsKSWeibull2siden",n.use,".RData", sep="") # KS 2-sided
fname.reg.Ord1<-paste(path.stem,"resultsSOWeibull1siden",n.use,".RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"resultsSOWeibull2siden",n.use,".RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# KS 1sided
load(fname.reg.KS1)
results.alpha.KS1side<-results.alpha.wb  # alpha0
results.prob.dropF.KS1side<-results.prob.dropF.wb  # type I error without dropping
results.prob.dropT.KS1side<-results.prob.dropT.wb  # type I error with dropping
results.biasF.KS1side<-results.biasF.wb   # bias results without dropping
results.biasT.KS1side<-results.biasT.wb   # bias results with dropping


# make room for KS2 (for good measure)
rm(results.prob.dropF.wb, results.prob.dropT.wb,results.alpha.wb, results.biasF.wb, results.biasT.wb)


# KS 2-sided
load(fname.reg.KS2)
results.alpha.KS2side<-results.alpha.wb
results.prob.dropF.KS2side<-results.prob.dropF.wb  # type I error without dropping
results.prob.dropT.KS2side<-results.prob.dropT.wb
results.biasF.KS2side<-results.biasF.wb   # bias results without dropping
results.biasT.KS2side<-results.biasT.wb   # bias results with dropping

rm(results.prob.dropF.wb, results.prob.dropT.wb,results.alpha.wb, results.biasF.wb, results.biasT.wb)


# SO 1-sided
load(fname.reg.Ord1)
results.alpha.SO1side<-results.alpha.wbord  # alpha0
results.prob.dropF.SO1side<-results.prob.dropF.wbord  # type I error without dropping
results.prob.dropT.SO1side<-results.prob.dropT.wbord 
results.biasF.SO1side<-results.biasF.wbord   # bias without dropping
results.biasT.SO1side<-results.biasT.wbord

# make room for SO2
rm(results.alpha.wbord,results.prob.dropF.wbord,results.prob.dropT.wbord, results.biasF.wbord, results.biasT.wbord)


# SO 2-sided
load(fname.reg.Ord2)
results.alpha.SO2side<-results.alpha.wbord  # alpha0
results.prob.dropF.SO2side<-results.prob.dropF.wbord  # type I error without dropping
results.prob.dropT.SO2side<-results.prob.dropT.wbord
results.biasF.SO2side<-results.biasF.wbord   # bias without dropping
results.biasT.SO2side<-results.biasT.wbord


rm(results.alpha.wbord,results.prob.dropF.wbord,results.prob.dropT.wbord, results.biasF.wbord, results.biasT.wbord)


# gather the vectors for output
list(alpha=c(results.alpha.KS2side, 
        results.alpha.KS1side,
        results.alpha.SO1side,
        results.alpha.SO2side), 
     
     typeI=c(results.prob.dropF.KS2side[1:20], results.prob.dropT.KS2side[1:20],
     results.prob.dropF.KS1side[1:20], results.prob.dropT.KS1side[1:20],
     results.prob.dropF.SO1side[1:20],results.prob.dropT.SO1side[1:20],
     results.prob.dropF.SO2side[1:20], results.prob.dropT.SO2side[1:20]),
     
     bias=c(
       results.biasF.KS2side, results.biasT.KS2side,
       results.biasF.KS1side, results.biasT.KS1side,
       results.biasF.SO1side, results.biasT.SO1side,
       results.biasF.SO2side, results.biasT.SO2side
     )
     
     )

}
