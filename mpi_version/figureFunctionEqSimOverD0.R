loadAllIterations<-function(path.stem,discfun.stem,n.use,discfun,tag,envr=.GlobalEnv){
  
  n.prior.sets<-1000
  
  fname1<-paste0(path.stem,discfun.stem,n.use,1,".RData") 
  load(fname1)
  
  
  if(discfun=="equiv") {
    
    # load and assign first iteration to objects
    
    results.alpha<-results.alpha.equiv  # alpha0
    results.prob.dropF<-results.prob.dropF.equiv  # type I error without dropping
    results.prob.dropT<-results.prob.dropT.equiv  # type I error with dropping
    results.biasF<-results.biasF.equiv   # bias results without dropping
    results.biasT<-results.biasT.equiv   # bias results with dropping
    results.SDF<-results.SDF.equiv   # SD results without dropping
    results.SDT<-results.SDT.equiv   # SD results with dropping
  

   # load remaining n.prior.sets -1 iterations

   for(i in 2:n.prior.sets) {
    
    fnamei<-paste0(path.stem,discfun.stem,n.use,i,".RData") 
    
    load(fnamei)
    
    results.alpha<-accumResults(x=list(results.alpha,results.alpha.equiv))[[2]]
    results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.equiv))[[2]]
    results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.equiv))[[2]]
    results.biasF<-accumResults(x=list(results.biasF, results.biasF.equiv))[[2]]
    results.biasT<-accumResults(x=list(results.biasT, results.biasT.equiv))[[2]]
    
   }  # end for
   
  } # end if equiv
  
  if(discfun=="wbord") {

    # load and assign first iteration to objects
    
    results.alpha<-results.alpha.wbord  # alpha0
    results.prob.dropF<-results.prob.dropF.wbord  # type I error without dropping
    results.prob.dropT<-results.prob.dropT.wbord  # type I error with dropping
    results.biasF<-results.biasF.wbord   # bias results without dropping
    results.biasT<-results.biasT.wbord   # bias results with dropping
    results.SDF<-results.SDF.wbord   # SD results without dropping
    results.SDT<-results.SDT.wbord   # SD results with dropping
    
    # load remaining n.prior.sets -1 iterations
    
    for(i in 2:n.prior.sets) {
      
      fnamei<-paste0(path.stem,discfun.stem,n.use,i,".RData") 
      
      load(fnamei)
      
      results.alpha<-accumResults(x=list(results.alpha,results.alpha.wbord))[[2]]
      results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.wbord))[[2]]
      results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.wbord))[[2]]
      results.biasF<-accumResults(x=list(results.biasF, results.biasF.wbord))[[2]]
      results.biasT<-accumResults(x=list(results.biasT, results.biasT.wbord))[[2]]
      
    }  # end for
    
    
    } # end if wbord

  
  # Same for all similarity measures:
  
  # assign to global environment
  
  assign(paste0("results.alpha.",tag,n.use),
         matrix(results.alpha/n.prior.sets,nc=4,byrow = T), envir = envr)
  assign(paste0("results.prob.dropF.",tag,n.use),
         matrix(results.prob.dropF/n.prior.sets,nc=4,byrow = T), envir = envr) # type I error without dropping
  assign(paste0("results.prob.dropT.",tag,n.use),
         matrix(results.prob.dropT/n.prior.sets,nc=4,byrow = T), envir = envr) # type I error with dropping
  assign(paste0("results.biasF.",tag,n.use),
         matrix(results.biasF/n.prior.sets,nc=4,byrow = T), envir = envr) # bias results without dropping
  assign(paste0("results.biasT.",tag,n.use),
         matrix(results.biasT/n.prior.sets,nc=4,byrow = T), envir = envr) # bias results with dropping
  assign(paste0("results.SDF.",tag,n.use),
         matrix(results.SDF/n.prior.sets,nc=4,byrow = T), envir = envr) # SD results without dropping
  assign(paste0("results.SDT.",tag,n.use),
         matrix(results.SDT/n.prior.sets,nc=4,byrow = T), envir = envr) # SD results with dropping
  
}



figureFunctionEqSimOverDo<-function(n.use, path.stem) {

# With Equivalence Similarity measure, we have EQ 1, 
# as well as PP (posterior prob) 1-sided
  
  

# Get EQ1 iterations
loadAllIterations(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,"equiv",tag="EQ1side")


# Get EQ2 iterations
loadAllIterations(path.stem,"resultsEQ1sideDeltaPonly0.2maxalpha1n",n.use,"equiv",tag="EQ2side")
  
  
# Get SO 1-sided iterations
loadAllIterations(path.stem, "resultsSOWeibull1siden",n.use,"wbord",tag="SO1side")


# Get SO 2-sided iterations
loadAllIterations(path.stem, "resultsSOWeibull2siden",n.use,"wbord",tag="SO2side")


# gather the vectors for output
# list(alpha=c(results.alpha.EQ2side, 
#         results.alpha.EQ1side,
#         results.alpha.SO1side,
#         results.alpha.SO2side), 
#      
#      typeI=c(results.prob.dropF.EQ2side[1:20], results.prob.dropT.EQ2side[1:20],
#      results.prob.dropF.EQ1side[1:20], results.prob.dropT.EQ1side[1:20],
#      results.prob.dropF.SO1side[1:20],results.prob.dropT.SO1side[1:20],
#      results.prob.dropF.SO2side[1:20], results.prob.dropT.SO2side[1:20]),
#      
#      bias=c(
#        results.biasF.EQ2side, results.biasT.EQ2side,
#        results.biasF.EQ1side, results.biasT.EQ1side,
#        results.biasF.SO1side, results.biasT.SO1side,
#        results.biasF.SO2side, results.biasT.SO2side
#      ),
#      
#      sdF=c(
#        results.SDF.EQ2side[1:20],
#        results.SDF.EQ1side[1:20],
#        results.SDF.SO1side[1:20],
#        results.SDF.SO2side[1:20]
#      )
#      
#      )

}
