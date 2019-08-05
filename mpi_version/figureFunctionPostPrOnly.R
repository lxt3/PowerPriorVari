
figureFunctionPPOnly<-function(n.use, path.stem, max.alpha) {

# Uses posterior probability only as the discount function, 
# SO has 1 and 2-sided results
  
  
# same for all characteristics
fname.reg.EQ1<-paste(path.stem,"powerEQ1sideDeltaPonly0.04maxalpha", max.alpha, "n",n.use,".RData", sep="") # there's no "side" here, need to change
fname.reg.EQ2<-paste(path.stem,"powerEQ1sideDeltaPonly0.08maxalpha", max.alpha, "n",n.use,".RData", sep="") 
fname.reg.EQ3<-paste(path.stem,"powerEQ1sideDeltaPonly0.12maxalpha", max.alpha, "n",n.use,".RData", sep="") 
fname.reg.EQ4<-paste(path.stem,"powerEQ1sideDeltaPonly0.16maxalpha", max.alpha, "n",n.use,".RData", sep="") 

fname.reg.Ord1<-paste(path.stem,"powerSOWeibull1sidePonlymaxalpha", max.alpha,"n", n.use,".RData", sep="")  # Stoch ord 1-sided (it's not Weibull, need to change)
fname.reg.Ord2<-paste(path.stem,"powerSOWeibull2sidePonlymaxalpha", max.alpha, "n",n.use,".RData", sep="")  # Stoch ord 2-sided


# load and assign to objects

# EQ 1
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


# EQ 2
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


# EQ 3
load(fname.reg.EQ3)
results.alpha.EQ3side<-results.alpha.equiv
results.prob.dropF.EQ3side<-results.prob.dropF.equiv  # type I error without dropping
results.prob.dropT.EQ3side<-results.prob.dropT.equiv
results.biasF.EQ3side<-results.biasF.equiv   # bias results without dropping
results.biasT.EQ3side<-results.biasT.equiv   # bias results with dropping
results.SDF.EQ3side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ3side<-results.SDT.equiv   # SD results with dropping

rm(results.prob.dropF.equiv, results.prob.dropT.equiv,results.alpha.equiv, results.biasF.equiv, results.biasT.equiv,
   results.SDF.equiv, results.SDT.equiv)

# EQ 4
load(fname.reg.EQ4)
results.alpha.EQ4side<-results.alpha.equiv
results.prob.dropF.EQ4side<-results.prob.dropF.equiv  # type I error without dropping
results.prob.dropT.EQ4side<-results.prob.dropT.equiv
results.biasF.EQ4side<-results.biasF.equiv   # bias results without dropping
results.biasT.EQ4side<-results.biasT.equiv   # bias results with dropping
results.SDF.EQ4side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ4side<-results.SDT.equiv   # SD results with dropping

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
list(alpha=c(
        results.alpha.EQ1side[1:20], 
        results.alpha.EQ2side[1:20],
        results.alpha.EQ3side[1:20],
        results.alpha.EQ4side[1:20],
        results.alpha.SO1side[1:20],
        results.alpha.SO2side[1:20]), 
     
     typeI=c(
       results.prob.dropF.EQ1side[1:20], results.prob.dropT.EQ1side[1:20],
       results.prob.dropF.EQ2side[1:20], results.prob.dropT.EQ2side[1:20],
       results.prob.dropF.EQ3side[1:20], results.prob.dropT.EQ3side[1:20],
       results.prob.dropF.EQ4side[1:20], results.prob.dropT.EQ4side[1:20],
       
       results.prob.dropF.SO1side[1:20],results.prob.dropT.SO1side[1:20],
       results.prob.dropF.SO2side[1:20], results.prob.dropT.SO2side[1:20]),
     
     bias=c(
       results.biasF.EQ1side[1:20], results.biasT.EQ1side[1:20],
       results.biasF.EQ2side[1:20], results.biasT.EQ2side[1:20],
       results.biasF.EQ3side[1:20], results.biasT.EQ3side[1:20],
       results.biasF.EQ4side[1:20], results.biasT.EQ4side[1:20],
       results.biasF.SO1side[1:20], results.biasT.SO1side[1:20],
       results.biasF.SO2side[1:20], results.biasT.SO2side[1:20]
     ),
     
     sdF=c(
       results.SDF.EQ1side[1:20],
       results.SDF.EQ2side[1:20],
       results.SDF.EQ3side[1:20],
       results.SDF.EQ4side[1:20],
       results.SDF.SO1side[1:20],
       results.SDF.SO2side[1:20]
     )
     
     )

}


figureFunctionExt<-function(n.use, path.stem,max.alpha) {
  
  # same for all characteristics
  fname.reg.EQ1<-paste(path.stem,"powerEQ1sideDeltaPonly0.04maxalpha", max.alpha, "n",n.use,"Ext.RData", sep="") # there's no "side" here, need to change
  fname.reg.EQ2<-paste(path.stem,"powerEQ1sideDeltaPonly0.06maxalpha", max.alpha, "n",n.use,"Ext.RData", sep="") 
  fname.reg.EQ3<-paste(path.stem,"powerEQ1sideDeltaPonly0.08maxalpha", max.alpha, "n",n.use,"Ext.RData", sep="") 
  fname.reg.EQ4<-paste(path.stem,"powerEQ1sideDeltaPonly0.1maxalpha", max.alpha, "n",n.use,"Ext.RData", sep="") 
  
  fname.reg.Ord1<-paste(path.stem,"powerSOWeibull1sidePonlymaxalpha", max.alpha,"n", n.use,"Ext.RData", sep="")  # Stoch ord 1-sided (it's not Weibull, need to change)
  fname.reg.Ord2<-paste(path.stem,"powerSOWeibull2sidePonlymaxalpha", max.alpha, "n",n.use,"Ext.RData", sep="")  # Stoch ord 2-sided
  
  
  # load and assign to objects
  
  # EQ1 
  load(fname.reg.EQ1)
  results.SDF.EQ1<-results.SDF.equiv   # SD without dropping
  
  # make room for EQ2 (for good measure)
  rm(results.SDF.equiv)
  
  
  # EQ2
  load(fname.reg.EQ2)
  results.SDF.EQ2<-results.SDF.equiv   # SD  without dropping
  rm(results.SDF.equiv)
  
  # EQ3
  load(fname.reg.EQ3)
  results.SDF.EQ3<-results.SDF.equiv   # SD  without dropping
  rm(results.SDF.equiv)

    # EQ4
  load(fname.reg.EQ4)
  results.SDF.EQ4<-results.SDF.equiv   # SD  without dropping
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
    results.SDF.EQ1, 
    results.SDF.EQ2, results.SDF.EQ3,results.SDF.EQ4,
    results.SDF.SO1side, 
    results.SDF.SO2side
  )
  
  )
  
}
