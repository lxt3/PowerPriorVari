### Figure 1

op <- par(mar=c(1.5,1,1,.25),mai=c(.2,.0,.2,.0),mgp=c(3,0,0))#,
         # font.main="TimesRoman")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))


# Figure 1a

x<-seq(-2, 4.0,0.01)
y<-dnorm(x,mean=1.0)

plot(x,y, type="l", xlab="", ylab="",bty="n", xaxt="n", 
     yaxt="n", xlim=c(-3.0, 8.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main=expression(paste( bold("(a) Current data better than prior (i.e., "), bold(theta > theta[0]), bold(")"))),cex.main=1,font=2)
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

legend(x=3.5, y=.35, fill=c(2,5,4,3), legend=c("larger n", "...", "...", "smaller n"))


## Figure 1b

x<-seq(-4.0,2.0,0.01)
y<-dnorm(x,mean=-1.0)
plot(x,y, type="l", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-6.0, 6.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main=expression(paste( bold("(a) Current data worse than prior (i.e., "), bold(theta < theta[0]), bold(")"))),cex.main=1,font=2)

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

plot(xx,yy, type="l", xlab="",#"p",
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
path.stem<-paste0(getwd(),"/output/singleD0alpha0.5/")
max.alpha<-1#0.5#1
source("figureFunctionPostPrOnly.R")

# get results for each n
res.100<-figureFunctionPPOnly(100, path.stem, max.alpha=max.alpha)
#res.50<-figureFunctionEqSim(50, path.stem)
#res.25<-figureFunctionPPOnly(25, path.stem, max.alpha=max.alpha)

# fixed max alpha0=1
fixed.1<-paste0(path.stem,"powerFixedmaxalpha1n100.RData")
load(fixed.1)
results.prob.dropF.fixed1<-results.prob.dropF.equiv  # type I error without dropping
results.biasF.fixed1<-results.biasF.equiv   # bias results without dropping

# fixed max alpha0=0.5
fixed.0.5<-paste0(path.stem,"powerFixedmaxalpha0.5n100.RData")
load(fixed.0.5)
results.prob.dropF.fixed0.5<-results.prob.dropF.equiv  # type I error without dropping
results.biasF.fixed0.5<-results.biasF.equiv   # bias results without dropping

# fixed max alpha0=0
results.prob.dropF.fixed0<-power.t.test(n=100,type = "one.sample",delta = c(0,.1,.2,.3,.4),sig.level = 0.025, alternative = "one.sided")$power


### Figure 3 (a and b)

# extract alpha0 part
raten100<-res.100$alpha
#raten50<-res.50$alpha
#raten100<-res.25$alpha

# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6), 
                 percent=as.character(rep(c(100,75,50,25), 5*6)),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4),1),
                 #Size=rep(c("100","050","025"),each=11*4*4),
                 rate=
                   c(raten100#,raten50,raten25
                    ))



# Figure 3 (All similarity measures)

library(lattice)
source("stripFunctionsv3R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1

xyplot(rate~mu|similarity,data=sims,groups=percent,subscripts = TRUE,
      # subset=(sims$similarity=="cKS-2" | sims$similarity=="bKS-1"),
       layout=c(3,2),as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.25)), 
                   x=list(at=c(0,.1,.2, .3, .4)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), 
       strip=my.strip5a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1





## Figure 4 (type I error rate and power)

# get results for typeI error rate
raten100<-res.100$typeI
# raten50<-res.50$typeI
# raten100<-res.25$typeI


# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6*2), 
                 percent=as.character(rep(c(100,75,50,25), 5*6*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),6*1),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4*2),1),
                # Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100#,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA



# Figure 4a (All similarity measures: for informational purposes)

library(lattice)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       layout=c(4,3),
       as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "Power",
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       #strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



## Figure power SO 1-sided, 2-sided (with fixed alpha0 superimposed)

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO2" ),  # "aS02" is 2-sided
       layout=c(2,1), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "Power",
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0, col="purple",pch=20,type="b",lty=2)
           panel.key(c("1.0","0.5","0"), col=c(6,"dark gray","purple"),points=F,border=T,
                     columns=3, corner=c(0.025,0.93),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
          # panel.key(c(" "),col="purple", points=F,lines=T, lty=2,border=T, columns=1, corner = c(.8,.4),
           #          title=expression(paste("Non-inform")),cex.title=.85, cex=.85)
         }
       }
)

trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


## Figure power SO 1-sided, 2-sided (with fixed alpha0 superimposed) All on one figure

# get type I error rates
typeI<-list(as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="aSO1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="aSO1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="aSO2", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="aSO2", "rate"],3))
)
typeI<-lapply(typeI, function(x) {x[is.na(x)]<-" "; x})


library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO1" | sims$similarity=="aSO2"),  
       layout=c(2,2), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "Power",
       
       # scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
       #             x=list(at=c(0,.1,.2, .3, .4), 
       #                    labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
       #             tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
                   x=list(limits=c(-0.05, .4),
                          at=c(-.05,0,.1,.2, .3, .4), 
                          labels=c(" ","  0.0","0.1","0.2 ","0.3 ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       
       strip=my.strip6b,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()%%2) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0, col="purple",pch=20,type="b",lty=2)
           panel.key(c("1.0","0.5","0"), col=c(6,"dark gray","purple"),points=F,border=T,
                     columns=3, corner=c(0.95,0.1), #corner=c(0.025,0.9),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
         }
         panel.key(typeI[[panel.number()]], columns=1, corner=c(.06,.9), cex=.68, points=F, border=F, 
                   col=c(1,4,3,2), title=c("Type I error rate"), cex.title=.7)
         
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1




## Figure power EQ 0.04, 0.06, 0.08, 0.1 (with fixed alpha0)

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="EQ4" ),  # delta=0.04 is EQ1... delta=0.1 is EQ4
       layout=c(2,1), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "Power",
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.025,0.85),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
         }
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


## Figure power Delta region 0.04 and 0.08 (with fixed alpha0 superimposed) All on one figure

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

# get type I error rates
typeI<-list(as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="EQ1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ2", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="EQ2", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ3", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="EQ3", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ4", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="yes" & sims$mu==0.0 & sims$similarity=="EQ4", "rate"],3))
)
typeI<-lapply(typeI, function(x) {x[is.na(x)]<-" "; x})


trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       #subset=(sims$similarity=="EQ1" | sims$similarity=="EQ3"),  
       subset=(sims$similarity!="aSO1" & sims$similarity!="aSO2"),  
       #layout=c(2,2), as.table=TRUE,
       layout=c(2,4), as.table=TRUE,
       
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = "Power",
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.2)), 
                   x=list(limits=c(-0.05, .4),
                     at=c(-.05,0,.1,.2, .3, .4), 
                          labels=c(" ","  0.0","0.1","0.2 ","0.3 ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       #strip=my.strip6c,
       strip=my.strip6d,
       
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if((panel.number()%%2)) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.prob.dropF.fixed0, col="purple",pch=20,type="b",lty=2)
           panel.key(c("1.0","0.5","0"), col=c(6,"dark gray","purple"),points=F,border=T,
                     columns=3, corner=c(0.95,0.1), #corner=c(0.025,0.9),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.82, cex=.82)
         }
           panel.key(typeI[[panel.number()]], columns=1, corner=c(.06,.9), cex=.68, points=F, border=F, 
                     col=c(1,4,3,2), title=c("Type I error rate"), cex.title=.7)

       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



#### Figure 6 (bias)

# get results for bias
raten100<-res.100$bias
#raten50<-res.50$bias
#raten25<-res.25$bias


# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6*2), 
                 percent=as.character(rep(c(100,75,50,25), 5*6*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),6*1),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4*2),1),
                 # Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100#,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA

##n=100 (All similarity measures)
source("stripFunctionsv3.R")
require(lattice)

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       layout=c(4,3),as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(theta^"*"), ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(tick.number=6), x=list(at=c(0,.1,.2,.3,.4), 
                                                 labels=c(" 0","0.1","0.2", "0.3","0.4 ")), 
                   cex=.7, tck=c(1,0), relation="same",alternating=c(1,1)), 
       strip=my.strip9a,
       panel = function(x, y,groups,subscripts) {
         panel.grid(h = 0, v = 0)
         panel.xyplot(x, y, pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts = subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")#loess,
       }#,
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


## Figure Bias SO 1-sided (with fixed alpha0 superimposed)

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO1" ),  
       layout=c(2,1), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.16,.11), at=seq(-.16,.11,by=0.02)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.6,0.85),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
         }
         panel.abline(h=0)
       }
)

trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



## Figure Bias SO 2-sided (with fixed alpha0 superimposed)

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO2" ),  
       layout=c(2,1), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.1,.1), at=seq(-.1,.1,by=0.02)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.6,0.85),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
         }
         panel.abline(h=0)
       }
)

trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



## Figure Bias Delta similarity (with fixed alpha0 superimposed)

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="EQ4" ),  
       layout=c(2,1), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.1,.1), at=seq(-.1,.1,by=0.02)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6a,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.6,0.85),
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
         }
         panel.abline(h=0)
       }
)

trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1

## Figure bias Delta region 0.04 and 0.08 (with fixed alpha0 superimposed) All on one figure

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="EQ1" | sims$similarity=="EQ4"),  
       layout=c(2,2), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.06,.06), at=seq(-.06,.06,by=0.02)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip9c,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1 | panel.number()==3) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.8,0.85), 
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.8, cex=.8)
         }
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


## Figure bias SO measures (with fixed alpha0 superimposed) All on one figure

library(lattice)
library(latticeExtra)
source("stripFunctionsv3.R")

trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
       subset=(sims$similarity=="aSO1" | sims$similarity=="aSO2"),  
       layout=c(2,2), as.table=TRUE,
       key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
                       columns=4, space="top",
                       title="Percent of D used",cex.title=.85), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.16,.10), at=seq(-.16,.10,by=0.04)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
       strip=my.strip6b,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
         panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
                         panel.groups=panel.xyplot, type="l")
         if(panel.number()==1 | panel.number()==3) {
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
           panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
           panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
                     columns=2, corner=c(0.8,0.9), 
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.8, cex=.8)
         }
       }
)
trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



#### Table 1 SD comparison uses a different function 

### Average of ratios calculation

load("~/MDIC/simulations/output/SDResults/powerSOWeibull2sidePonlymaxalpha1n100.RData")
usetwice<-results.sdi.wbord
usetwice.1<-lapply(usetwice, unlist, recursive=FALSE)

load("~/MDIC/simulations/output/SDResults/powerSOWeibull2sidePonlymaxalpha1n100Ext.RData")
nousetwice<-results.sdi.wbord
nousetwice.1<-lapply(nousetwice, unlist, recursive=FALSE)


sdresSO2<-matrix(colMeans(mapply(`/`, nousetwice.1,usetwice.1)),nc=4,byrow=F)


res<-rbind(sdres04,sdres06,sdres08,sdres10,sdresSO1,sdresSO2)




sd.summarize.yes<-sims[sims$twice.D1=="yes",]
sd.summarize.no<-sims[sims$twice.D1=="no",]


library(htmlTable)
stem<-sd.summarize.no[,c(1:2,4)]
stem$ratio<-txtRound(sd.summarize.no$rate/sd.summarize.yes$rate,digits = 4)

res<-reshape(stem, idvar=c("similarity","mu"),direction = "wide",timevar = c("percent"), v.names = "ratio")
levels(res$similarity)<-c("Stoc Order 1-sided", "Stoc Order 2-sided", "Delta Sim 0.04", "Delta Sim 0.06",
                          "Delta Sim 0.08", "Delta Sim 0.10") 

res.tab<-htmlTable(res, rnames=FALSE, 
                   header=c("mu", "Similarity Measure", "100%", "75%", "50%", "25%"))
