loadAllIterations<-function(path.stem,discfun.stem,n.use,discfun,tag,external=FALSE,envr=.GlobalEnv){
  
  n.prior.sets<-1000
  ext<-if(external) "Ext.RData" else ".RData"
  
  fname1<-paste0(path.stem,discfun.stem,n.use,1,ext) 
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
    
    fnamei<-paste0(path.stem,discfun.stem,n.use,i,ext) 
    
    load(fnamei)
    
    results.alpha<-accumResults(x=list(results.alpha,results.alpha.equiv))[[2]]
    results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.equiv))[[2]]
    results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.equiv))[[2]]
    results.biasF<-accumResults(x=list(results.biasF, results.biasF.equiv))[[2]]
    results.biasT<-accumResults(x=list(results.biasT, results.biasT.equiv))[[2]]
    results.SDF<-accumResults(x=list(results.SDF, results.SDF.equiv))[[2]]
    results.SDT<-accumResults(x=list(results.SDT, results.SDT.equiv))[[2]]
    
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
      
      fnamei<-paste0(path.stem,discfun.stem,n.use,i,ext) 
      
      load(fnamei)
      
      results.alpha<-accumResults(x=list(results.alpha,results.alpha.wbord))[[2]]
      results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.wbord))[[2]]
      results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.wbord))[[2]]
      results.biasF<-accumResults(x=list(results.biasF, results.biasF.wbord))[[2]]
      results.biasT<-accumResults(x=list(results.biasT, results.biasT.wbord))[[2]]
      results.SDF<-accumResults(x=list(results.SDF, results.SDF.wbord))[[2]]
      results.SDT<-accumResults(x=list(results.SDT, results.SDT.wbord))[[2]]
      
      
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



figureFunctionEqSimOverDo<-function(n.use, path.stem,external=FALSE) {

# With Equivalence Similarity measure, we have EQ 1, 
# as well as PP (posterior prob) 1-sided (EQ2)
  
  

# Get EQ1 iterations
loadAllIterations(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,"equiv",tag="EQ1side",external)


# Get EQ2 iterations
loadAllIterations(path.stem,"resultsEQ1sideDeltaPonly0.2maxalpha1n",n.use,"equiv",tag="EQ2side",external)
  
  
# Get SO 1-sided iterations
loadAllIterations(path.stem, "resultsSOWeibull1siden",n.use,"wbord",tag="SO1side",external)


# Get SO 2-sided iterations
loadAllIterations(path.stem, "resultsSOWeibull2siden",n.use,"wbord",tag="SO2side",external)


}


