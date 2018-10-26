############### function for running simulations #################

if(!dir.exists("output")) dir.create("output")

path.to.files<-paste0(getwd(),"/output/")

seed.<-3

np<-6

n0=100
n<-n0 

external=T#F#T

OC<-"typeI" #"power" # "typeI"
#mus=c(-1,-.75,-.5, -.25, -.1,0,.1,.25,.5,.75,1)  # type I and bias and alpha0
mus=c(-1,-.75,-.5, -.25, -.1)  # Ext
#mus=c(-.35, -.25, -.1,0) # power

mu0<- 0
power.null<- -0.5 # null for power simulations

discfun<- "wbord" #"wb" # change function loss for 1-side vs. 2-side
two.sided<-T  # two-sided discount function

max_alpha<-1


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

sinkfname<-paste0(fname.core,n0,".txt")


nsim<-15000/(np-1)
nmcmc<-45000
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
results.biasT<-vector(length=dims) # ,T
results.biasF<-vector(length=dims) # T
results.SDT<-vector(length=dims) # ,T
results.SDF<-vector(length=dims) # 


if(is.null(sinkfname)){
  sinkfname<-paste(fname.core,n0,".txt",sep="")
}



set.seed(seed.) # used in paper (same seed for every parameter configuation)

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
 

