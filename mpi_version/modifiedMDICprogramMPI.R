# One-arm trial (OPC) example

# set different seeds per processor (diff=TRUE)
#comm.set.seed(seed., diff=TRUE)  
comm.set.seed(seed.init, diff=TRUE)  

# Generate nsim current data sets and compute posterior inference using prior D0
# Definition of FUN is in functionsSimulation.R
ret<-task.pull(1:nworkers,FUN, nsim=nsim, n=n, mu=mu, percent=percent,null=null,
          prob.H1=prob.H1,nmcmc=nmcmc,max_alpha=max_alpha,D0=D0)


if(comm.rank()==0){
  ret.jobs<-lapply(ret,function(x){
    c(res1=mean(x$res1), res2=mean(x$res2), alpha=mean(x$alpha), bias1=mean(x$bias1), 
      bias2=mean(x$bias2), sd1=mean(x$sd.1), sd2=mean(x$sd.2))
  })
  ret.jobs<-Reduce("+",ret.jobs)/np # means
  
  # write results to sink
  sink(sinkfname,split=T, append=T) 
  cat("mu: ",mu, "\n")
  cat("null: ",null, "\n")
  cat("percent: ",percent, "\n")
  cat("External?: ",external, "\n")
  
  print(ret.jobs)
  sink()
  
}



