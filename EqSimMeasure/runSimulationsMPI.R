############### function for running simulations for a single D0 #################
# to run, install R package pbdmpi (it already exists on HPC).
# Also, install an mpi program on your desktop, if you opt not to use the HPC
#
# then, on HPC, at bash shell prompt type:
# > mpiexec -np 6 Rscript "runSimulationsMPI.R"

# (If mpiexec does not work, use mpirun instead)

# on a PC with several cores, type in a command shell (after installing an mpi program, plus R package)

# $ mpiexec -np 6 Rscript.exe "runSimulationsMPI.R"

if(!dir.exists("output")) dir.create("output")

path.to.files<-paste0(getwd(),"/output/EQ/")

seed.<-seed.init<-3 # seed used for current draft of paper

# num processors used in the mpi (1 proc is the master, np-1 are the workers)
np<-6
nworkers<-np-1

n0=25
n<-n0 

external=F#T  # T is used to get Figures 7 in paper; otherwise set to F

OC<- "typeI"  #  "power" # "typeI"  # are you calculating type I error, alpha0, and bias or power?
mus=c(-1,-.75,-.5, -.25, -.1,0,.1,.25,.5,.75,1)  # type I and bias and alpha0
#mus=c(-1,-.75,-.5, -.25, -.1)  # use for External, even though you calculate type I error, etc
#mus=c(-.35, -.25, -.1,0) # power

mu0<- 0
power.null<- -0.5 # null for power simulations

discfun<-  "equiv" #"wbord" #"wb"  # what is the general discount fcn: 
                                    # "equiv" = similarity region disc fun
                                    # "wb" = KS discount fcn (old, not used anymore)
                                    # "wbord" = stochastic ordering (as per Haddad et al. 2017)
two.sided<-F#T#F#T  # two-sided discount function or not (equiv is always one-sided, not matter what you put here)

post.prob.only<-T#F # used with equivalence similarity measure; T = identity discount fcn
delta<-.2 # used with equivalence similarity measure

max_alpha<-1#0.50

# These if statements define any disc fcn parameter, and set the core name of output files
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

# the sink file collects results in human readable form; not for use by R in processing results
sinkfname<-paste0(fname.core,n0,".txt")

# number of total simulations = 15,000; number of sims per worker = nsim
nsim<-15000/nworkers
nmcmc<-40000#45000 # num mcmc iterations
prob.H1<-.975

percents=rev(c(.25,.5,.75,1))


if(external==T) {
  fname<-paste0(fname.core, n0, "Ext.RData") 
}else { 
  fname<-paste0(fname.core, n0, ".RData")
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
results.biasT<-vector(length=dims) # 
results.biasF<-vector(length=dims) # 
results.SDT<-vector(length=dims) # 
results.SDF<-vector(length=dims) # 


if(is.null(sinkfname)){
  sinkfname<-paste(fname.core,n0,".txt",sep="")
}


# seed2.=5 was used to get a D0 data set for n=25 that had a sample mean close to 0, true prior mean
if(n0==25) seed2.<-5 else seed2.<-seed.
set.seed(seed2.) # used in paper (same seed for every parameter configuation)

# generate D0 
D0<-rnorm(mean=mu0, n=n0)

# reset seed for all to seed.
set.seed(seed.) # used in paper (same seed for every parameter configuation)


# run nsim simulations per parameter configuration (percent and mu value)
# main mpi part
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


finalize(mpi.finalize = TRUE)

# end mpi part
