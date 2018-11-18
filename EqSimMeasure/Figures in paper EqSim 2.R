### Figure 1

op <- par(mar=c(1.5,1,1,.25),mai=c(.2,.0,.2,.0),mgp=c(3,0,0),
          font.main="TimesRoman")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))


# Figure 1a

x<-seq(-2, 4.0,0.01)
y<-dnorm(x,mean=1.0)

plot(x,y, type="l", xlab="", ylab="",bty="n", xaxt="n", 
     yaxt="n", xlim=c(-3.0, 8.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(a) Current data better than prior",cex.main=1)
axis(1,  at=c(-3, 0, 1.5, 3.5, 7.0), lwd.ticks = -1,
     labels=c(NA,0, NA,NA,NA))

x1<-seq(0.0,6.0,0.01)
y1<-dnorm(x1,mean=1.0)

x2<-seq(-2.0,7.0,0.01)
y2<-dnorm(x2,mean=1.0,sd=2)

x3<-seq(-2.0,7.0,0.01)
y3<-dnorm(x3,mean=1.0,sd=1.5)

x4<-seq(-2.0,7.0,0.01)
y4<-dnorm(x4,mean=1.0,sd=1.25)

xvalues<-x[x>=0]

segments(x0=c(xvalues,x1[x1>2.5]), y0=0, x1=c(xvalues,x1[x1>2.5]),
         y1=dnorm(c(xvalues,x1[x1>2.5]),mean = 1.0)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x2[x2>3]), y0=0, x1=c(xvalues,x2[x2>3]),
         y1=dnorm(c(xvalues,x2[x2>3]),mean = 1.0, sd=2)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x3[x3>3]), y0=0, x1=c(xvalues,x3[x3>3]),
         y1=dnorm(c(xvalues,x3[x3>3]),mean = 1.0, sd=1.5)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x4[x4>3]), y0=0, x1=c(xvalues,x4[x4>3]),
         y1=dnorm(c(xvalues,x4[x4>3]),mean = 1.0, sd=1.25)-.006,lty=1, col="lightgray")

lines(x,y,col=1,lwd=2)
lines(x1,y1,col=2,lwd=2)
lines(x2,y2,col=3,lwd=2)
lines(x3,y3,col=4,lwd=2)
lines(x4,y4,col=5,lwd=2)

segments(x0=0, y0=0, x1=0,y1=dnorm(0),lty=2)


## Figure 1b

x<-seq(-4.0,2.0,0.01)
y<-dnorm(x,mean=-1.0)
plot(x,y, type="l", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-6.0, 6.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(b) Current data worse than prior",cex.main=1)
axis(1,  at=c(-5.5, 0, 2, 5.0), lwd.ticks = -1,#lwd=-1,padj=0,
     labels=c(NA,0,NA,NA))#labels=F)

x1<-seq(-2.0,4.0,0.01)
y1<-dnorm(x1,mean=-1.0)

x2<-seq(-5.0,5,0.01)
y2<-dnorm(x2,mean=-1.0,sd=2)

x3<-seq(-5.0,5,0.01)
y3<-dnorm(x3,mean=-1.0,sd=1.5)

x4<-seq(-5.0,5,0.01)
y4<-dnorm(x4,mean=-1.0,sd=1.25)

xvalues<-x[x>=0]

segments(x0=c(xvalues,x1[x1>2]), y0=0, x1=c(xvalues,x1[x1>2]),
         y1=dnorm(c(xvalues,x1[x1>2]),mean = -1.0)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x2[x2>2]), y0=0, x1=c(xvalues,x2[x2>2]),
         y1=dnorm(c(xvalues,x2[x2>2]),mean = -1.0, sd=2)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x3[x3>2]), y0=0, x1=c(xvalues,x3[x3>2]),
         y1=dnorm(c(xvalues,x3[x3>2]),mean = -1.0, sd=1.5)-.006,lty=1, col="lightgray")
segments(x0=c(xvalues,x4[x4>2]), y0=0, x1=c(xvalues,x4[x4>2]),
         y1=dnorm(c(xvalues,x4[x4>2]),mean = -1.0, sd=1.25)-.006,lty=1, col="lightgray")

lines(x,y,col=1,lwd=2)
lines(x1,y1,col=2,lwd=2)
lines(x2,y2,col=3,lwd=2)
lines(x3,y3,col=4,lwd=2)
lines(x4,y4,col=5,lwd=2)

segments(x0=0, y0=0, x1=0,y1=dnorm(0),lty=2)


# Figure 1c

x<-seq(-2,4.0,0.01)
y<-dnorm(x,mean=1.0)

delta.<-1.2

plot(x,y, type="l", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-3.0, 8.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(c) Current data better than prior",cex.main=1)
axis(1,  at=c(-3, -delta., 0, delta., 6.0), lwd.ticks = -1,
     labels=c(NA, expression(-delta),0, expression(delta),NA))

x1<-seq(-2.0,4.0,0.01)
y1<-dnorm(x1,mean=1.0)

x2<-seq(-3.0,5.0,0.01)
y2<-dnorm(x2,mean=1.0,sd=2)

x3<-seq(-3.0,5.0,0.01)
y3<-dnorm(x3,mean=1.0,sd=1.5)

x4<-seq(-3.0,5.0,0.01)
y4<-dnorm(x4,mean=1.0,sd=1.25)

ind<-c(x>=-delta.& x<=delta.)
xvalues<-x[ind]
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = 1.0)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = 1.0, sd=2)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = 1.0, sd=1.5)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = 1.0, sd=1.25)-.006,lty=1, col="lightgray")

lines(x,y,col=1,lwd=2)
lines(x1,y1,col=2,lwd=2)
lines(x2,y2,col=3,lwd=2)
lines(x3,y3,col=4,lwd=2)
lines(x4,y4,col=5,lwd=2)

segments(x0=-delta., y0=0, x1=-delta.,y1=dnorm(0),lty=2)
segments(x0=delta., y0=0, x1=delta.,y1=dnorm(0),lty=2)


## Figure 1d

x<-seq(-4.0,2.0,0.01)
y<-dnorm(x,mean=-1.0)

delta.<-1.2

plot(x,y, type="l", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-6.0, 6.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(d) Current data worse than prior",cex.main=1)
axis(1,  at=c(-5.5, -delta., 0, delta., 4.5), lwd.ticks = -1,#lwd=-1,padj=0,
     labels=c(NA, expression(-delta),0, expression(delta),NA))


x1<-seq(-4.0,3.0,0.01)
y1<-dnorm(x1,mean=-1.0)

x2<-seq(-5.0,3.0,0.01)
y2<-dnorm(x2,mean=-1.0,sd=2)

x3<-seq(-5.0,3,0.01)
y3<-dnorm(x3,mean=-1.0,sd=1.5)

x4<-seq(-5.0,3,0.01)
y4<-dnorm(x4,mean=-1.0,sd=1.25)

ind<-c(x>=-delta.& x<=delta.)

segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = -1.0)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = -1.0, sd=2)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = -1.0, sd=1.5)-.006,lty=1, col="lightgray")
segments(x0=xvalues, y0=0, x1=xvalues,
         y1=dnorm(xvalues,mean = -1.0, sd=1.25)-.006,lty=1, col="lightgray")

lines(x,y,col=1,lwd=2)
lines(x1,y1,col=2,lwd=2)
lines(x2,y2,col=3,lwd=2)
lines(x3,y3,col=4,lwd=2)
lines(x4,y4,col=5,lwd=2)

segments(x0=-delta., y0=0, x1=-delta.,y1=dnorm(0),lty=2)
segments(x0=delta., y0=0, x1=delta.,y1=dnorm(0),lty=2)


par(op)

#### Figure 2 in MDIC paper (alpha functions)

xx<-seq(0,1,.01)
yy<-pweibull(xx, shape = 3, scale = .65)

plot(xx,yy, type="l", xlab="p",
     #expression(paste(p, " = ", p,"(",theta, " < ", theta[0],")")),
     ylab=expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
     bty="n",xaxt="n", yaxt="n")
axis(1)
axis(2, lwd.ticks = -1)

yy<-ifelse(xx<.5, pweibull(xx, shape = 1.5, scale = .4), pweibull(1-xx, shape = 1.5, scale = .4))
lines(xx,yy, type="l", lty=2)

legend(x=0.1, y=1, legend = c("1-sided Weibull(0.65, 3.0)", "2-sided Weibull(0.40, 1.5)"),lty=c(1,2),cex=0.9)


#### Lattice figures

#path.stem<-paste0(getwd(),"/PaperFigures/")  # change to this when I transfer RDatas to PaperFigures
path.stem<-paste0(getwd(),"/output/")

# get results for each n
res.100<-figureFunctionEqSim(100, path.stem)
res.50<-figureFunctionEqSim(50, path.stem)
res.25<-figureFunctionEqSim(25, path.stem)


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



# Figure 3a (EQ measures)

library(lattice)
source("stripFunctionsEqSim.R")
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
source("stripFunctionsEqSim.R")
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



# Figure 4a (EQ measures)

library(lattice)
source("stripFunctionsEqSim.R")
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="cKS-2" | sims$similarity=="bKS-1") & 
                 ((sims$Size=="025") | (sims$Size=="100")) 
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
source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1

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
source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1

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
raten100<-figureFunctionPowerEqSim(100, path.stem)
raten50<-figureFunctionPowerEqSim(50, path.stem)
raten25<-figureFunctionPowerEqSim(25, path.stem)

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


# Figure 5a (EQ measures)

source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity*Size,data=sims,groups=percent,subscripts = TRUE,
       subset=((sims$similarity=="cKS-2" | sims$similarity=="bKS-1") & ((sims$Size=="025") | (sims$Size=="100")) 
       ),
       layout=c(4,2),#as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.9), 
       xlab = expression(theta^"*"), ylab = "power", ylim=c(0,1.1), 
       
       scales=list(y=list(at=c(.2,.4,.6,.8,1)), 
                   x=list(at=c(-.35,-.25,-.1, 0), labels=c("-0.35","-0.25","-0.10", "0")), 
                   relation="same",alternating=c(1,1)),
       #plot.args=list(mgp=c(1,1,0)),
       strip=my.strip6a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)


# Figure 5b (SO measures)

source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0, axis.panel=0)) # default is 1,1
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
trellis.par.set(layout.heights = list(axis.xlab.padding=1, axis.panel=1)) # default is 1,1


#### Figure 7 SD comparison uses a different function too

#path.stem<-paste0(getwd(),"/PaperFigures/")  # change to this when I transfer RDatas to PaperFigures
path.stem<-paste0(getwd(),"/output/")


# Get exteranl data from figureFunctionExt
raten100Ext<-figureFunctionExtEqSim(100, path.stem)$sdF
raten50Ext<-figureFunctionExtEqSim(50, path.stem)$sdF
raten25Ext<-figureFunctionExtEqSim(25, path.stem)$sdF

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
source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0, axis.panel=0)) # default is 1,1

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
trellis.par.set(layout.heights = list(axis.xlab.padding=1, axis.panel=1)) # default is 1,1


## n=25
source("stripFunctionsEqSim.R")
require(lattice)
trellis.par.set(layout.heights = list(axis.xlab.padding=0, axis.panel=0)) # default is 1,1

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

