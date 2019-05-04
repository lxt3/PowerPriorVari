
figureFunctionPowerEqSim<-function(n.use, path.stem) {

# same for all characteristics
fname.reg.EQ1<-paste(path.stem,"powerEQ1sideDelta0.2maxalpha1n",n.use,".RData", sep="") # EQ 1-sided
fname.reg.EQ2<-paste(path.stem,"powerEQ2sideDelta0.2maxalpha1n",n.use,".RData", sep="") # EQ 2-sided
fname.reg.Ord1<-paste(path.stem,"powerSOWeibull1siden",n.use,".RData", sep="")  # Stoch ord 1-sided
fname.reg.Ord2<-paste(path.stem,"powerSOWeibull2siden",n.use,".RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# EQ 1sided
load(fname.reg.EQ1)
results.prob.dropF.EQ1side<-results.prob.dropF.equiv  # power without dropping
results.prob.dropT.EQ1side<-results.prob.dropT.equiv  # power with dropping


# make room for EQ2 (for good measure)
rm(results.prob.dropF.equiv, results.prob.dropT.equiv)


# EQ 2-sided
load(fname.reg.EQ2)
results.prob.dropF.EQ2side<-results.prob.dropF.equiv  # power without dropping
results.prob.dropT.EQ2side<-results.prob.dropT.equiv

rm(results.prob.dropF.equiv, results.prob.dropT.equiv)


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

     c(results.prob.dropF.EQ2side, results.prob.dropT.EQ2side,
     results.prob.dropF.EQ1side, results.prob.dropT.EQ1side,
     results.prob.dropF.SO1side,results.prob.dropT.SO1side,
     results.prob.dropF.SO2side, results.prob.dropT.SO2side)
     
}
