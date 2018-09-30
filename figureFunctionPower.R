
figureFunctionPower<-function(n.use, path.stem) {

# same for all characteristics
fname.reg.KS1<-paste(path.stem,"powerKSWeibull1siden",n.use,".RData", sep="") # KS 1-sided
fname.reg.KS2<-paste(path.stem,"powerKSWeibull2siden",n.use,".RData", sep="") # KS 2-sided
fname.reg.Ord1<-paste(path.stem,"powerSOWeibull1siden",n.use,".RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"powerSOWeibull2siden",n.use,".RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# KS 1sided
load(fname.reg.KS1)
results.prob.dropF.KS1side<-results.prob.dropF.wb  # power without dropping
results.prob.dropT.KS1side<-results.prob.dropT.wb  # power with dropping


# make room for KS2 (for good measure)
rm(results.prob.dropF.wb, results.prob.dropT.wb)


# KS 2-sided
load(fname.reg.KS2)
results.prob.dropF.KS2side<-results.prob.dropF.wb  # power without dropping
results.prob.dropT.KS2side<-results.prob.dropT.wb

rm(results.prob.dropF.wb, results.prob.dropT.wb)


# SO 1-sided
load(fname.reg.Ord1)
results.prob.dropF.SO1side<-results.prob.dropF.wbord  # power without dropping
results.prob.dropT.SO1side<-results.prob.dropT.wbord 

# make room for SO2
rm(results.prob.dropF.wbord,results.prob.dropT.wbord)


# SO 2-sided
load(fname.reg.Ord2)
results.prob.dropF.SO2side<-results.prob.dropF.wbord  # power without dropping
results.prob.dropT.SO2side<-results.prob.dropT.wbord


rm(results.prob.dropF.wbord,results.prob.dropT.wbord)


# gather the vectors for output

     c(results.prob.dropF.KS2side, results.prob.dropT.KS2side,
     results.prob.dropF.KS1side, results.prob.dropT.KS1side,
     results.prob.dropF.SO1side,results.prob.dropT.SO1side,
     results.prob.dropF.SO2side, results.prob.dropT.SO2side)
     
}
