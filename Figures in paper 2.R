
#### Figure 2 in MDIC paper (alpha functions)

x<-seq(0,1,.01)
y<-pweibull(x, shape = 12, scale = .9)

plot(x,y, type="l",xlab=expression(1 - KS), 
     ylab=expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
     #ylab=paste("\\alpha_0"),
     bty="n",xaxt="n", 
     yaxt="n")

y<-pweibull(x, shape = 20, scale = .9)

lines(x,y, type="l", lty=2)
axis(1)
axis(2, lwd.ticks = -1)

legend(x=0.15, y=0.8, legend = c("Weibull(0.9, 12)", "Weibull(0.9, 20)"),lty=c(1,2),cex=.9)


# figure 2b

xx<-seq(0,1,.01)
yy<-pweibull(xx, shape = 3, scale = .65)

plot(xx,yy, type="l",#xlab=expression(paste(p,group("(", list(theta,"<", theta[0]),")" ))), 
     xlab=expression(paste(p, " = ", p,"(",theta, " < ", theta[0],")")),
    # ylab=expression(paste(alpha[0],group("(",list(theta[0]^{i},theta[1]^{i}),")"))),
    ylab=expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
     bty="n",xaxt="n", 
     yaxt="n")
axis(1)
axis(2, lwd.ticks = -1)

yy<-ifelse(xx<.5, pweibull(xx, shape = 1.5, scale = .4), pweibull(1-xx, shape = 1.5, scale = .4))
lines(xx,yy, type="l", lty=2)

legend(x=0.1, y=1, legend = c("1-sided Weibull(0.65, 3.0)", "2-sided Weibull(0.40, 1.5)"),lty=c(1,2),cex=0.9)


#### Lattice figures

#path.stem<-paste0(getwd(),"/PaperFigures/")  # change to this when I transfer RDatas to PaperFigures
path.stem<-paste0(getwd(),"/output/")

# get results for each n
res.100<-figureFunction(100, path.stem)
res.50<-figureFunction(50, path.stem)
res.25<-figureFunction(25, path.stem)


### Figure 3 (a and b)

# extract alpha0 part
raten100<-res.100$alpha
raten50<-res.50$alpha
raten25<-res.25$alpha

# create the whole data frame
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5, -.25, -.1,0,.1,.25,.5,.75,1),each=4),4*3), 
                 percent=as.character(rep(c(100,75,50,25), 11*4*3)),
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=11*4),3),
                 Size=rep(c("100","050","025"),each=11*4*4),
                 
                 rate=
                   c(raten100,raten50,raten25
                    ))



# Figure 3a (KS measures)

library(lattice)
source("stripFunctions.R")
xyplot(rate~mu|Size*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="cKS-2" | sims$similarity=="bKS-1"),
       layout=c(3,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.25)), 
                   #x=list(at=c(-1,-.75,-.5, -.25, 0, .25,.5,.75,1)),
                   x=list(at=c(-1,-.5,  0, .5,1)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=my.strip5a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)



# Figure 3b (Stochastic ordering measures)

library(lattice)
source("stripFunctions.R")
xyplot(rate~mu|Size*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO1" | sims$similarity=="aSO2"),
       layout=c(3,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.25)), 
                   #x=list(at=c(-1,-.75,-.5, -.25, 0, .25,.5,.75,1)),
                   x=list(at=c(-1,-.5,  0, .5,1)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=my.strip5,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)


## Figure 4 (type I error rates)

# get results for typeI error rate
raten100<-res.100$typeI
raten50<-res.50$typeI
raten25<-res.25$typeI


# create the whole data frame
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5, -.25, -.1),each=4),4*3*2), 
                 percent=as.character(rep(c(100,75,50,25), 5*4*3*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),4*3),
                 
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=5*4*2),3),
                 Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA



# Figure 4a (KS measures)

library(lattice)
source("stripFunctions.R")
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="cKS-2" | sims$similarity=="bKS-1") & ((sims$Size=="025") | (sims$Size=="100")) 
               ),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(paste(theta^"*")), 
       ylab = "type I error rate",
       
       scales=list(y=list(limits=c(0.0,.25), at=seq(0.0,.25,by=0.05)), 
                   x=list(at=c(-1,-.75,-.5, -.25,-0.1), labels=c("-1","-0.75","-0.50","-0.25","-0.10"),cex=.7),
                   tck=c(1,0),relation="same",alternating=c(1,1)), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
       }
)

# Figure 4b (SO measures)

require(lattice)
source("stripFunctions.R")
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="aSO2" | sims$similarity=="aSO1") & ((sims$Size=="025") | (sims$Size=="100"))),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(paste(theta^"*")), 
       ylab = "type I error rate",
       
       scales=list(y=list(limits=c(0.0,.25), at=seq(0.0,.25,by=0.05)), 
                   #x=list(at=c(-1,-.75,-.5, -.25, 0, .25,.5,.75,1)),
                   x=list(at=c(-1,-.75,-.5, -.25,-.1), labels=c("-1","-0.75","-0.50","-0.25","-0.10"),cex=.7),
                   tck=c(1,0),relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=my.strip6b,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)

#### Figure 6 (bias)

# get results for bias
raten100<-res.100$bias
raten50<-res.50$bias
raten25<-res.25$bias


# make the data frame
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5,-.25,-.1,0,.1,.25,.5,.75,1),each=4),16),         # 4
                 percent=as.character(rep(c(100,75,50,25), 176)),    # 4
                 discard.D1=rep(rep(c("no", "yes"), each=44),8),    # 2
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=88),2),  # 4
                 Size=rep(c("100","025"),each=352),             # 3
                 rate=
                   c( 
                     raten100, raten25
                   ))

##n=100
source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="100",
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.5,0,.5,1)), 
                   relation="same",alternating=c(1,1)), 
       strip=my.strip9a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)


##n=25
source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="025",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.5,0,.5,1)), 
                   relation="same",alternating=c(1,1)), 
       strip=my.strip9a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
       }#,
)


#### Figure 5 Power uses a different figureFunction

#path.stem<-paste0(getwd(),"/PaperFigures/")  # change to this when I transfer RDatas to PaperFigures
path.stem<-paste0(getwd(),"/output/")

# get results for each n
raten100<-figureFunctionPower(100, path.stem)
raten50<-figureFunctionPower(50, path.stem)
raten25<-figureFunctionPower(25, path.stem)

# Make data frame
sims<-data.frame(mu=rep(rep(c(-.35,-.25,-.1, 0),each=4),8*3),         # 4
                 percent=as.character(rep(c(100,75,50,25), 32*3)),    # 4
                 discard.D1=rep(rep(c("no", "yes"), each=16),4*3),    # 2
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=32),3),  # 4
                 Size=rep(c("100","050","025"),each=128),             # 3
                 rate=
                   c( 
                     raten100, raten50, raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA


# Figure 5a (KS measures)

source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="cKS-2" | sims$similarity=="bKS-1") & ((sims$Size=="025") | (sims$Size=="100")) 
       ),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = "power",
       
       scales=list(y=list(tick.number=5), x=list(at=c(-.35,-.25,-.1, 0), labels=c("-0.35","-0.25","-0.10", "0")), 
                   relation="same",alternating=c(1,1)), 
       strip=my.strip6a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)

# Figure 5b (SO measures)

source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="aSO2" | sims$similarity=="aSO1") & ((sims$Size=="025") | (sims$Size=="100")) 
       ),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = "power",
       
       scales=list(y=list(at=c(0,.2,.4,.6,.8,1)),#tick.number=6), 
                   x=list(at=c(-.35,-.25,-.1, 0), labels=c("-0.35","-0.25","-0.10", "0")), 
                   relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=my.strip6b,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)


#### Figure 7 SD comparison uses a different function too

#path.stem<-paste0(getwd(),"/PaperFigures/")  # change to this when I transfer RDatas to PaperFigures
path.stem<-paste0(getwd(),"/output/")


# Get exteranl data from figureFunctionExt
raten100Ext<-figureFunctionExt(100, path.stem)$sdF
raten50Ext<-figureFunctionExt(50, path.stem)$sdF
raten25Ext<-figureFunctionExt(25, path.stem)$sdF

# get results for sdF from internal data
raten100<-res.100$sdF
raten50<-res.50$sdF
raten25<-res.25$sdF


# Make data frame
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5,-.25,-.1),each=4),8*2),         # 4
                 percent=as.character(rep(c(100,75,50,25), 80)),    # 4
                 twice.D1=rep(rep(c("yes", "no"), each=80),2), # each=20),4*2),    # 2
                 similarity=rep(rep(c("cKS-2","bKS-1","aSO1","aSO2"),each=20),4),  # 4
                 Size=rep(c("100","025"),each=160),             # 3
                 rate=
                   c( 
                     raten100,raten100Ext,
                     raten25,raten25Ext
                   ))

## n=100
source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|twice.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset= sims$Size=="100",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = expression(paste("SD of ", theta)),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.75,-.5,-.25,-.1),cex=.7,
                                                 labels=c("-1","-0.75","-0.50","-0.25","-0.10")), 
                   relation="same",alternating=c(1,1)), 
       strip=my.strip6aaa,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)

## n=25
source("stripFunctions.R")
require(lattice)
xyplot(rate~mu|twice.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=sims$Size=="025",
       layout=c(4,2),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = expression(paste("SD of ", theta)),
       
       scales=list(y=list(tick.number=6), x=list(at=c(-1,-.75,-.5,-.25,-.1), 
                                                 labels=c("-1","-0.75","-0.50","-0.25","-0.10"), cex=.7), 
                   relation="same",alternating=c(1,1)), 
       strip=my.strip6aaa,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)




############3 Appendix figures #########

# These are not as maintained, and results are not in usual directory

##### Figure A1 (type I error rate for max alpha = 50%)

# Figure A1a
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5, -.25, -.1),each=4),2), 
                 percent=as.character(rep(c(100,75,50,25), 10)),
                 discard.D1=rep(c("no", "yes"), each=20),
                 rate=
                   c(0.035, 0.033, 0.031, 0.032, # mu = -1 drop=F
                     0.054, 0.049, 0.047, 0.046, # mu = -.75
                     0.092, 0.082, 0.080, 0.071, # mu=-0.5
                     0.093, 0.082, 0.054, 0.050, #mu = -0.25
                     0.031, 0.031, 0.028, 0.027, #mu = -0.1
                     
                     NA, 0.029, 0.029, 0.028, # mu = -1, drop=T
                     NA, 0.046, 0.040, 0.038, # mu = -.75
                     NA, 0.088, 0.067, 0.055, # mu= -.5
                     NA, 0.068, 0.054, 0.043, # mu=-0.25
                     NA, 0.015, 0.024, 0.026  #mu = -0.10
                   ))

require(lattice)
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       layout=c(2,1),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,columns=4, space="top",
                       title="Percent of D used",cex.title=1), 
       xlab = expression(theta^"*"), ylab = "type I error rate",
       
       scales=list(y=list(at=c(0,.05,.10,.15,.20,.25), limits=c(0,.25)), 
                   x=list(at=c(-1,-.75, -.5,-.25,-.10)), relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=strip.custom(strip.names = c(TRUE),strip.levels=c(T),
                          var.name=c(expression(paste("Discard ", D[1]))),
                          sep=expression(paste(": ")),
                          factor.levels=c(expression(paste("no")), 
                                          expression(paste("yes")))
       ),
       
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)

# Figure A1b

load("resultsKSWeibullmax502n25.RData")
sims<-data.frame(mu=rep(rep(c(-1,-.75,-.5, -.25, -.1),each=4),2), 
                 percent=as.character(rep(c(100,75,50,25), 10)),
                 discard.D1=rep(c("no", "yes"), each=20),
                 rate=results.prob.dropF.wb, results.prob.dropT.wb
)
# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA

require(lattice)
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       layout=c(2,1),
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,columns=4, space="top",
                       title="Percent of D used",cex.title=1), 
       xlab = expression(theta^"*"), ylab = "type I error rate",
       
       scales=list(y=list(at=c(0,.05,.10,.15,.20,.25), limits=c(0.0,.25)), 
                   x=list(at=c(-1,-.75, -.5,-.25,-.10)), relation="same",alternating=c(1,1)), #x=list(at=c(-1,-.5,-.25)
       strip=strip.custom(strip.names = c(TRUE),strip.levels=c(T),
                          var.name=c(expression(paste("Discard ", D[1]))),
                          sep=expression(paste(": ")),
                          factor.levels=c(expression(paste("no")), 
                                          expression(paste("yes")))
       ),
       
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)

