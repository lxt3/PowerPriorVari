############### function for running simulations #################
## This script runs one configuration of 15,000 simulations for each mu* and percent, 
## given a single D0.
## Fanning the script out to 1000 nodes will return 1000 averages over each set of 
## 15K simulations for 1000 different D0's

if(!dir.exists("output")) dir.create("output")

path.to.files<-paste0(getwd(),"/output/EQ/")

# random seed business: 
# seed.init will be different for each D0, but the same for each configuration
ran.init.sym<-Sys.getenv("RAND_INIT")
if(ran.init.sym=="") ran.init<-0 else ran.init<-as.numeric(ran.init.sym)
seed.init <-ran.init+as.numeric(Sys.getenv("SGE_TASK_ID"))
seed.init                          ### print out the seed for reproduction if necessary
set.seed(seed.init)

sge=Sys.getenv("SGE_TASK_ID")

## number processors for the 15,000 simulations, given a single D0, and this configuration
np<-6
nworkers<-np-1

### The configuration parameters
n0=100
n<-n0 

external=T#F#T

OC<- "typeI" #"power" # "typeI"
#mus=c(-1,-.75,-.5, -.25, -.1,0,.1,.25,.5,.75,1)  # type I and bias and alpha0
mus=c(-1,-.75,-.5, -.25, -.1)  # External
#mus=c(-.35, -.25, -.1,0) # power

mu0<- 0
power.null<- -0.5 # null for power simulations

discfun<-  "wbord" #"equiv" #"wbord" #"wb" 
two.sided<-T#F#T  # two-sided discount function

post.prob.only<-F#T#F # used with equivalence similarity measure
delta<-.2 # used with equivalence similarity measure

max_alpha<-1#0.50

### end of configuration parameters


# output file stems

if(discfun=="wb" && OC=="typeI" ){
  if(two.sided){
    fname.core<-paste0(path.to.files,"resultsKSWeibull2siden")
    ws=.9; wsh=12;  # if weibull_scale=99 that represents pvalue equals weight
  } else{
    fname.core<-paste0(path.to.files,"resultsKSWeibull1siden")
    ws=.9; wsh=20;
  }
}

if(discfun=="wb" && OC=="power" ){
  if(two.sided){
    fname.core<-paste0(path.to.files,"powerKSWeibull2siden")
    ws=.9; wsh=12; # if weibull_scale=99 that represents pvalue equals weight
  } else{
    fname.core<-paste0(path.to.files,"powerKSWeibull1siden")
    ws=.9; wsh=20;
  }
}


if(discfun=="wbord" && OC=="typeI" ){
  if(two.sided){
    fname.core<-paste0(path.to.files,"resultsSOWeibull2siden")
    ws=.4; wsh=1.5; # if weibull_scale=99 that represents pvalue equals weight
  } else{
    fname.core<-paste0(path.to.files,"resultsSOWeibull1siden")
    ws=.65; wsh=3; # if weibull_scale=99 that represents pvalue equals weight
  }
}

if(discfun=="wbord" && OC=="power" ){
  if(two.sided){
    fname.core<-paste0(path.to.files,"powerSOWeibull2siden")
    ws=.4; wsh=1.5; # if weibull_scale=99 that represents pvalue equals weight
  } else{
    fname.core<-paste0(path.to.files,"powerSOWeibull1siden")
    ws=.65; wsh=3; # if weibull_scale=99 that represents pvalue equals weight
  }
}

if(discfun=="equiv" && OC=="typeI" ){
  Ponly<-ifelse(post.prob.only,"Ponly","")
    fname.core<-paste0(path.to.files,"resultsEQ1sideDelta",Ponly,delta,"maxalpha",max_alpha,"n")
    ws=.65; wsh=3; # if weibull_scale=99 that represents pvalue equals weight
}

if(discfun=="equiv" && OC=="power" ){
  Ponly<-ifelse(post.prob.only,"Ponly","")
    fname.core<-paste0(path.to.files,"powerEQ1sideDelta",Ponly,delta,"maxalpha",max_alpha,"n")
    ws=.65; wsh=3; # if weibull_scale=99 that represents pvalue equals weight
}


# sink file name
sinkfname<-paste0(fname.core,n0,sge,".txt")


nsim<-15000/nworkers
nmcmc<-40000#45000
prob.H1<-.975

percents=rev(c(.25,.5,.75,1))


if(external==T) {
  fname<-paste0(fname.core, n0, sge, "Ext.RData") 
}else { 
  fname<-paste0(fname.core, n0, sge, ".RData")
}

# source functions
source('functionsSimulation.R')


# Results vectors
n.i<-length(mus)
n.j<-length(percents)
dims<-n.i*n.j
results.prob.dropT<-vector(length=dims) 
results.prob.dropF<-vector(length=dims) # 
results.alpha<-vector(length=dims) # 
results.biasT<-vector(length=dims) # ,T
results.biasF<-vector(length=dims) # T
results.SDT<-vector(length=dims) # ,T
results.SDF<-vector(length=dims) # 


if(is.null(sinkfname)){
  sinkfname<-paste(fname.core,n0, sge, ".txt",sep="")
}


# generate D0 
D0<-rnorm(mean=mu0, n=n0)


# run nsim simulations per parameter configuration (percent and mu value)

suppressMessages(library(pbdMPI, quietly = TRUE))

init()

for(i. in 1:n.i){ # loop over mu (theta) values
  mu<-mus[i.]
  
  if(comm.rank()==0) cat("mu: ",mu, "\n")
  
  # type I error or power
  if(OC=="typeI")  null<-mu else null<- power.null
  
  for(j. in 1:n.j){ # loop over percents
    percent<-percents[j.]
    
    if(comm.rank()==0) cat("percent: ",percent, "\n")
    
    
    source("modifiedMDICprogramMPI.R")

    
if(comm.rank()==0){
    # put mean results in vectors
    results.prob.dropF[n.j*i. - n.j + j.]<-ret.jobs["res1"]
    results.prob.dropT[n.j*i. - n.j + j.]<-ret.jobs["res2"]
    results.alpha[n.j*i. - n.j + j.]<-ret.jobs["alpha"]
    results.biasF[n.j*i. - n.j + j.]<-ret.jobs["bias1"]
    results.biasT[n.j*i. - n.j + j.]<-ret.jobs["bias2"]
    results.SDF[n.j*i. - n.j + j.]<-ret.jobs["sd1"]
    results.SDT[n.j*i. - n.j + j.]<-ret.jobs["sd2"]
}
  }
}

#finalize(mpi.finalize = TRUE)

if(comm.rank()==0){

 probF<-paste0("results.prob.dropF",".",discfun)
 probT<-paste0("results.prob.dropT",".",discfun)
 alpha<-paste0("results.alpha",".",discfun)
 SDT<-paste0("results.SDT",".",discfun)
 SDF<-paste0("results.SDF",".",discfun)
 biasT<-paste0("results.biasT",".",discfun)
 biasF<-paste0("results.biasF",".",discfun)
# 
 assign(probF,results.prob.dropF)
 assign(probT,results.prob.dropT)
 assign(alpha, results.alpha)
 assign(SDT, results.SDT)
 assign(SDF, results.SDF)
 assign(biasT, results.biasT)
 assign(biasF, results.biasF)
 
# 
# 
 save(list=c(probT, probF, alpha, SDT, SDF, biasT, biasF),
      file=fname)
}

finalize(mpi.finalize = TRUE)
 
