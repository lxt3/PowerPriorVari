loadAllPowerIterations<-function(path.stem,discfun.stem,n.use,discfun,tag,envr=.GlobalEnv){
  
  n.prior.sets<-1000

  fname1<-paste0(path.stem,discfun.stem,n.use,1,".RData") 
  load(fname1)
  
  
  if(discfun=="equiv") {
    
    # load and assign first iteration to objects
    
    results.prob.dropF<-results.prob.dropF.equiv  # power without dropping
    results.prob.dropT<-results.prob.dropT.equiv  # power with dropping


   # load remaining n.prior.sets -1 iterations

   for(i in 2:n.prior.sets) {
    
    fnamei<-paste0(path.stem,discfun.stem,n.use,i,".RData") 
    
    load(fnamei)
    
    results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.equiv))[[2]]
    results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.equiv))[[2]]

   }  # end for
   
  } # end if equiv
  
  if(discfun=="wbord") {

    # load and assign first iteration to objects
    
    results.prob.dropF<-results.prob.dropF.wbord  # type I error without dropping
    results.prob.dropT<-results.prob.dropT.wbord  # type I error with dropping

    # load remaining n.prior.sets -1 iterations
    
    for(i in 2:n.prior.sets) {
      
      fnamei<-paste0(path.stem,discfun.stem,n.use,i,".RData") 
      
      load(fnamei)
      
      results.prob.dropF<-accumResults(x=list(results.prob.dropF, results.prob.dropF.wbord))[[2]]
      results.prob.dropT<-accumResults(x=list(results.prob.dropT, results.prob.dropT.wbord))[[2]]
      
    }  # end for
    
    
    } # end if wbord

  
  # Same for all similarity measures:
  
  # assign to global environment
  
  assign(paste0("results.prob.dropF.",tag,n.use),
         matrix(results.prob.dropF/n.prior.sets,nc=4,byrow = T), envir = envr) # type I error without dropping
  assign(paste0("results.prob.dropT.",tag,n.use),
         matrix(results.prob.dropT/n.prior.sets,nc=4,byrow = T), envir = envr) # type I error with dropping
}



figureFunctionPowerEqSimOverDo<-function(n.use, path.stem) {

# With Equivalence Similarity measure, we have EQ 1, 
# as well as PP (posterior prob) 1-sided (EQ2)
  
  

# Get EQ1 iterations
loadAllPowerIterations(path.stem,"powerEQ1sideDelta0.2maxalpha1n",n.use,"equiv",tag="EQ1side")


# Get EQ2 iterations
loadAllPowerIterations(path.stem,"powerEQ1sideDeltaPonly0.2maxalpha1n",n.use,"equiv",tag="EQ2side")
  
  
# Get SO 1-sided iterations
loadAllPowerIterations(path.stem, "powerSOWeibull1siden",n.use,"wbord",tag="SO1side")


# Get SO 2-sided iterations
loadAllPowerIterations(path.stem, "powerSOWeibull2siden",n.use,"wbord",tag="SO2side")


}


