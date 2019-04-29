
path.stem<-paste0(getwd(),"/output/EQ2/")
#source("figureFunctionEqSim.R")
source('~/MDIC/simulations/figureFunctionEqSimOverD0.R')

require(plyr)
require(dplyr)
require(purrr)


# utility functions
accumResults<-partial(Reduce, f="+", accumulate=TRUE)
assignAccumResults<-function(y1,y2){ assign(y1,accumResults(x=list(y1,y2))[[2]]) }

# get results for each n
figureFunctionEqSimOverDo(n.use=25, path.stem)
figureFunctionEqSimOverDo(n.use=50, path.stem)
figureFunctionEqSimOverDo(n.use=100, path.stem)


### Figure 3 (a and b)

# extract alpha0 part
raten100<-c(results.alpha.EQ2side100, results.alpha.EQ1side100, results.alpha.SO1side100,results.alpha.SO2side100)
raten50<-c(results.alpha.EQ2side50, results.alpha.EQ1side50, results.alpha.SO1side50,results.alpha.SO2side50)
raten25<-c(results.alpha.EQ2side25, results.alpha.EQ1side25, results.alpha.SO1side25,results.alpha.SO2side25)
           

# create the whole data frame (note that this is a different structure than for the fixed D0 simulations)
sims<-data.frame(mu=rep(c(-1,-.75,-.5, -.25, -.1,0,.1,.25,.5,.75,1),4*4*3),
                 percent=as.character(rep(rep(c(100,75,50,25), each=11),4*3)),
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=11*4),3),
                 Size=rep(c("100","050","025"),each=11*4*4),
                 
                 rate=
                   c(raten100,raten50,raten25
                   ))


## all else same as for fixed D0

# Figure 3a (EQ measures)

library(lattice)
source("stripFunctionsEqSim.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1

xyplot(rate~mu|Size*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="cKS-2" | sims$similarity=="bKS-1"),
       layout=c(3,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.25)), 
                   #x=list(at=c(-1,-.75,-.5, -.25, 0, .25,.5,.75,1)),
                   x=list(at=c(-1,-.5,  0, .5,1)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), 
       strip=my.strip5a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=1:4,groups=groups,subscripts=subscripts)
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


# Figure 3b (Stochastic ordering measures)

library(lattice)
source("stripFunctionsEqSim.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|Size*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO1" | sims$similarity=="aSO2"),
       layout=c(3,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.25)), 
                   x=list(at=c(-1,-.5,  0, .5,1)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), 
       strip=my.strip5,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=1:4,groups=groups,subscripts=subscripts)
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=c(1,2,3,4),#c(1,4,3,2), 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


### Figure 4 (a and b)

# extract typeI part

raten25<-c(results.prob.dropF.EQ2side25[1:5,1:4], results.prob.dropT.EQ2side25[1:5,1:4],
           results.prob.dropF.EQ1side25[1:5,1:4], results.prob.dropT.EQ1side25[1:5,1:4],
           results.prob.dropF.SO1side25[1:5,1:4], results.prob.dropT.SO1side25[1:5,1:4],
           results.prob.dropF.SO2side25[1:5,1:4], results.prob.dropT.SO2side25[1:5,1:4])

raten50<-c(results.prob.dropF.EQ2side50[1:5,1:4], results.prob.dropT.EQ2side50[1:5,1:4],
           results.prob.dropF.EQ1side50[1:5,1:4], results.prob.dropT.EQ1side50[1:5,1:4],
           results.prob.dropF.SO1side50[1:5,1:4], results.prob.dropT.SO1side50[1:5,1:4],
           results.prob.dropF.SO2side50[1:5,1:4], results.prob.dropT.SO2side50[1:5,1:4])

raten100<-c(results.prob.dropF.EQ2side100[1:5,1:4], results.prob.dropT.EQ2side100[1:5,1:4],
           results.prob.dropF.EQ1side100[1:5,1:4], results.prob.dropT.EQ1side100[1:5,1:4],
           results.prob.dropF.SO1side100[1:5,1:4], results.prob.dropT.SO1side100[1:5,1:4],
           results.prob.dropF.SO2side100[1:5,1:4], results.prob.dropT.SO2side100[1:5,1:4])


# create the whole data frame (note mu and percent have different arrangements from fixed D0 arrangement)
sims<-data.frame(mu=rep(c(-1,-.75,-.5, -.25, -.1),4*4*3*2), 
                 percent=as.character(rep(rep(c(100,75,50,25), each=5),4*3*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),4*3),
                 
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=5*4*2),3),
                 Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA

# Figure 4a (EQ measures)

library(lattice)
source("stripFunctionsEqSim.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="cKS-2" | sims$similarity=="bKS-1") & 
                 ((sims$Size=="025") | (sims$Size=="100")) 
       ),
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "type I error rate",
       
       scales=list(y=list(limits=c(0.0,.25), at=seq(0.0,.25,by=0.05)), 
                   x=list(at=c(-1,-.75,-.5, -.25,-0.1), 
                          labels=c("   -1.0","-0.75","-0.50 ","-0.25  ","-0.1")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,subscripts=subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1

# Figure 4b (SO measures)

require(lattice)
source("stripFunctionsEqSim.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="aSO2" | sims$similarity=="aSO1") & ((sims$Size=="025") | (sims$Size=="100"))),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "type I error rate",
       
       scales=list(y=list(limits=c(0.0,.25), at=seq(0.0,.25,by=0.05)), 
                   x=list(at=c(-1,-.75,-.5, -.25,-.1), 
                          labels=c("   -1.0","-0.75","-0.50 ","-0.25   ","-0.1")),
                   tck=c(1,0),relation="same",alternating=c(1,1),cex=.7), 
       strip=my.strip6b,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,subscripts=subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



#### Figure 6 (bias)

# get results for bias
raten25<-c(results.biasF.EQ2side25, results.biasT.EQ2side25,
           results.biasF.EQ1side25, results.biasT.EQ1side25,
           results.biasF.SO1side25, results.biasT.SO1side25,
           results.biasF.SO2side25, results.biasT.SO2side25)
raten50<-c(results.biasF.EQ2side50, results.biasT.EQ2side50,
           results.biasF.EQ1side50, results.biasT.EQ1side50,
           results.biasF.SO1side50, results.biasT.SO1side50,
           results.biasF.SO2side50, results.biasT.SO2side50)
raten100<-c(results.biasF.EQ2side100, results.biasT.EQ2side100,
           results.biasF.EQ1side100, results.biasT.EQ1side100,
           results.biasF.SO1side100, results.biasT.SO1side100,
           results.biasF.SO2side100, results.biasT.SO2side100)


# make the data frame
sims<-data.frame(mu=rep(c(-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1),4*16),         # 4
                 percent=as.character(rep(rep(c(100,75,50,25), each=11),16)),    # 4
                 discard.D1=rep(rep(c("no", "yes"), each=44),8),    # 2
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=88),2),  # 4
                 Size=rep(c("100","025"),each=352),             # 3
                 rate=
                   c( 
                     raten100, raten25
                   ))

##n=100
source("stripFunctionsEqSim.R")
require(lattice)

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="100",
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(theta^"*"), ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.5,0,.5,1), 
                                                 labels=c(" -1.0","-0.5","0", "0.5","1.0 ")), 
                   cex=.7, tck=c(1,0), relation="same",alternating=c(1,1)), 
       strip=my.strip9a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,subscripts = subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


##n=25
source("stripFunctionsEqSim.R")
require(lattice)

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="025",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(theta^"*"), ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.5,0,.5,1), 
                                                 labels=c(" -1.0","-0.5","0", "0.5","1.0 ")), 
                   cex=.7, tck=c(1,0), relation="same",alternating=c(1,1)), 
       strip=my.strip9a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,subscripts = subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
       }#,
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



#### Figure 5 Power uses a different figureFunction


#### Figure 7 SD comparison 


# get results for sdF from internal data

raten25<-c(results.SDF.EQ2side25[1:5,1:4], results.SDF.EQ1side25[1:5,1:4], 
           results.SDF.SO1side25[1:5,1:4], results.SDF.SO2side25[1:5,1:4])
raten50<-c(results.SDF.EQ2side50[1:5,1:4], results.SDF.EQ1side50[1:5,1:4], 
           results.SDF.SO1side50[1:5,1:4], results.SDF.SO2side50[1:5,1:4])
raten100<-c(results.SDF.EQ2side100[1:5,1:4], results.SDF.EQ1side100[1:5,1:4], 
           results.SDF.SO1side100[1:5,1:4], results.SDF.SO2side100[1:5,1:4])


# Get exteranl data from external data

# get results for each n
figureFunctionEqSimOverDo(n.use=25, path.stem,external=TRUE)
figureFunctionEqSimOverDo(n.use=50, path.stem,external=TRUE)
figureFunctionEqSimOverDo(n.use=100, path.stem,external=TRUE)


raten25Ext<-c(results.SDF.EQ2side25[1:5,1:4], results.SDF.EQ1side25[1:5,1:4], 
              results.SDF.SO1side25[1:5,1:4], results.SDF.SO2side25[1:5,1:4])
raten50Ext<-c(results.SDF.EQ2side50[1:5,1:4], results.SDF.EQ1side50[1:5,1:4], 
              results.SDF.SO1side50[1:5,1:4], results.SDF.SO2side50[1:5,1:4])
raten100Ext<-c(results.SDF.EQ2side100[1:5,1:4], results.SDF.EQ1side100[1:5,1:4], 
              results.SDF.SO1side100[1:5,1:4], results.SDF.SO2side100[1:5,1:4])


# Make data frame (note different arrangement of mu and percent than with fixed D0)
sims<-data.frame(mu=rep(c(-1,-.75,-.5,-.25,-.1),4*8*2),         # 4
                 percent=as.character(rep(rep(c(100,75,50,25), each=5), 16)),    # 4
                 twice.D1=rep(rep(c("yes", "no"), each=80),2), # each=20),4*2),    # 2
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=20),4),  # 4
                 Size=rep(c("100","025"),each=160),             # 3
                 rate=
                   c( 
                     raten100,raten100Ext,
                     raten25,raten25Ext
                   ))

## n=100
source("stripFunctionsEqSim.R")
require(lattice)

trellis.par.set(layout.heights = list(axis.xlab.padding=0, axis.panel=0)) # default is 1,1
xyplot(rate~mu|twice.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="100",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(theta^"*"), ylab = expression(paste("SD of ", theta)),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.75,-.5,-.25,-.1),cex=.7,
                                                 labels=c("  -1","-0.75","-0.50","-0.25  "," -0.1")), 
                   tck=c(1,0), relation="same",alternating=c(1,1)), 
       strip=my.strip6aaa,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,subscripts = subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1, axis.panel=1)) # default is 1,1


## n=25
source("stripFunctionsEqSim.R")
require(lattice)

trellis.par.set(layout.heights = list(axis.xlab.padding=0, axis.panel=0)) # default is 1,1
xyplot(rate~mu|twice.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="025",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(theta^"*"), ylab = expression(paste("SD of ", theta)),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.75,-.5,-.25,-.1),cex=.7,
                                                 labels=c("  -1","-0.75","-0.50","-0.25  "," -0.1")), 
                   tck=c(1,0), relation="same",alternating=c(1,1)), 
       strip=my.strip6aaa,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,subscripts = subscripts,groups=groups,col=1:4)
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1, axis.panel=1)) # default is 1,1



#####################3 older

fname.reg.EQ1<-paste(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,1,".RData", sep="") # EQ 1-sided
fname.reg.EQ1<-paste(path.stem,"resultsEQ1sideDeltaPonly0.2maxalpha1n",n.use,1,".RData", sep="") # EQ 1-sided


# EQ 1sided
load(fname.reg.EQ1)
results.alpha.EQ1side<-results.alpha.equiv  # alpha0
results.prob.dropF.EQ1side<-results.prob.dropF.equiv  # type I error without dropping
results.prob.dropT.EQ1side<-results.prob.dropT.equiv  # type I error with dropping
results.biasF.EQ1side<-results.biasF.equiv   # bias results without dropping
results.biasT.EQ1side<-results.biasT.equiv   # bias results with dropping
results.SDF.EQ1side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ1side<-results.SDT.equiv   # SD results with dropping

n.prior.sets<-1000

for(i in 2:n.prior.sets) {

  fname.reg.EQ1<-paste(path.stem,"resultsEQ1sideDelta0.2maxalpha1n",n.use,i,".RData", sep="") # EQ 1-sided

  load(fname.reg.EQ1)
  #assignAccumResults(deparse(substitute(results.alpha.EQ1side)),results.alpha.equiv)
  results.alpha.EQ1side<-accumResults(x=list(results.alpha.EQ1side,results.alpha.equiv))[[2]]
  results.prob.dropF.EQ1side<-accumResults(x=list(results.prob.dropF.EQ1side, results.prob.dropF.equiv))[[2]]
  results.prob.dropT.EQ1side<-accumResults(x=list(results.prob.dropT.EQ1side, results.prob.dropT.equiv))[[2]]
 # assignAccumResults(results.biasF.EQ1side, results.biasF.equiv)
  results.biasF.EQ1side<-accumResults(x=list(results.biasF.EQ1side, results.biasF.equiv))[[2]]
  results.biasT.EQ1side<-accumResults(x=list(results.biasT.EQ1side, results.biasT.equiv))[[2]]
  
  }

matrix(results.alpha.EQ1side/n.prior.sets,nc=4,byrow = T)
matrix(results.alpha.equiv,nc=4,byrow = T)

matrix(results.biasF.EQ1side/n.prior.sets,nc=4,byrow = T)
matrix(results.biasF.equiv,nc=4,byrow = T)


results.SDF.EQ1side<-results.SDF.equiv   # SD results without dropping
results.SDT.EQ1side<-results.SDT.equiv   # SD results with dropping


# get results for each n
res.100<-figureFunctionEqSim(100, path.stem)
res.50<-figureFunctionEqSim(50, path.stem)
res.25<-figureFunctionEqSim(25, path.stem)
