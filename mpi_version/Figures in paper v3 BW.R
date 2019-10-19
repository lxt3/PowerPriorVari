### Figure 1

op <- par(mar=c(1.5,1,1,.25),mai=c(.2,.0,.2,.0),mgp=c(3,0,0))

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))


# Figure 1a

x<-seq(-2.0,7.0,0.01)#seq(-2, 4.0,0.01)
y<-dnorm(x,mean=1.0)

plot(x,y, type="n", xlab="", ylab="",bty="n", xaxt="n", 
     yaxt="n", xlim=c(-3.0, 8.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main=expression(paste( bold("(a) Current data better than prior (i.e., "), bold(theta > theta[0]), bold(")"))),cex.main=1.2,font=2)
axis(1,  at=c(-3, 0, 1.5, 3.5, 7.0), lwd.ticks = -1,
     labels=c(NA,0, NA,NA,NA),cex.axis=1.1)

x1<-seq(-2.0,7.0,0.01)
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


lines(x1,y1,lty=2,lwd=2)
lines(x2,y2,lty=3,lwd=2)
lines(x3,y3,lty=4,lwd=2)
lines(x4,y4,lty=5,lwd=2)

segments(x0=0, y0=0, x1=0,y1=dnorm(0),lty=2)

legend(x=3.5, y=.35, legend=c("larger n", "...", "...", "smaller n"),cex=1.1, lty=c(2,5,4,3),lwd=2)


## Figure 1b

x<-seq(-5.0,4.0,0.01)#seq(-4.0,2.0,0.01)
y<-dnorm(x,mean=-1.0)
plot(x,y, type="n", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-6.0, 6.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main=expression(paste( bold("(b) Current data worse than prior (i.e., "), bold(theta < theta[0]), bold(")"))),cex.main=1.2,font=2)

axis(1,  at=c(-5.5, 0, 2, 5.0), lwd.ticks = -1,#lwd=-1,padj=0,
     labels=c(NA,0,NA,NA), cex.axis=1.1)#labels=F)

x1<-seq(-5.0,4.0,0.01)
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


lines(x1,y1,lty=2,lwd=2)
lines(x2,y2,lty=3,lwd=2)
lines(x3,y3,lty=4,lwd=2)
lines(x4,y4,lty=5,lwd=2)

segments(x0=0, y0=0, x1=0,y1=dnorm(0),lty=2)


# Figure 1c

x<-seq(-3.0,4.0,0.01)#seq(-2,4.0,0.01)
y<-dnorm(x,mean=1.0)

delta.<-1.2

plot(x,y, type="n", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-3.0, 8.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(c) Current data better than prior",cex.main=1.2)
axis(1,  at=c(-3, -delta., 0, delta., 6.0), lwd.ticks = -1,
     labels=c(NA, expression(-delta),0, expression(delta),NA),cex.axis=1.1)

x1<-seq(-3.0,4.0,0.01)
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

lines(x1,y1,lty=2,lwd=2)
lines(x2,y2,lty=3,lwd=2)
lines(x3,y3,lty=4,lwd=2)
lines(x4,y4,lty=5,lwd=2)

segments(x0=-delta., y0=0, x1=-delta.,y1=dnorm(0),lty=2)
segments(x0=delta., y0=0, x1=delta.,y1=dnorm(0),lty=2)


## Figure 1d

x<-seq(-5.0,3.0,0.01)#seq(-4.0,2.0,0.01)
y<-dnorm(x,mean=-1.0)

delta.<-1.2

plot(x,y, type="n", xlab="", ylab="",bty="n",xaxt="n", 
     yaxt="n", xlim=c(-6.0, 6.0),lwd=2, ylim=c(0,.40),frame.plot=FALSE)
title(main="(d) Current data worse than prior",cex.main=1.2)
axis(1,  at=c(-5.5, -delta., 0, delta., 4.5), lwd.ticks = -1,#lwd=-1,padj=0,
     labels=c(NA, expression(-delta),0, expression(delta),NA),cex.axis=1.1)


x1<-seq(-5.0,3.0,0.01)
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

lines(x1,y1,lty=2,lwd=2)
lines(x2,y2,lty=3,lwd=2)
lines(x3,y3,lty=4,lwd=2)
lines(x4,y4,lty=5,lwd=2)

segments(x0=-delta., y0=0, x1=-delta.,y1=dnorm(0),lty=2)
segments(x0=delta., y0=0, x1=delta.,y1=dnorm(0),lty=2)


par(op)


############### Lattice figures  ###############

path.stem<-paste0(getwd(),"/output/PaperFigures/")  #path.stem<-paste0(getwd(),"/output/singleD0alpha0.5/")
max.alpha<-1.0#0.5
source("figureFunctionPostPrOnly.R")

# get results for each n
n.res<-100 # n.res = 101 if ratio of n0/n = 2, ow n.res = 100

#res.100<-figureFunctionPPOnly(100, path.stem, max.alpha=max.alpha)
res.100<-figureFunctionPPOnly(n.res, path.stem, max.alpha=max.alpha)  # the n=101 is actually n0=200/n=100

# For n=25, 50, not used in paper
#res.50<-figureFunctionEqSim(50, path.stem, max.alpha=max.alpha)
#res.25<-figureFunctionPPOnly(25, path.stem, max.alpha=max.alpha)

# fixed  alpha0=1
fixed.1<-paste0(path.stem,paste0("powerFixedmaxalpha1n",n.res,".RData"))
load(fixed.1)
results.prob.dropF.fixed1<-results.prob.dropF.equiv  # type I error without dropping
results.biasF.fixed1<-results.biasF.equiv   # bias results without dropping

# fixed  alpha0=0.5
fixed.0.5<-paste0(path.stem,paste0("powerFixedmaxalpha0.5n",n.res,".RData"))
load(fixed.0.5)
results.prob.dropF.fixed0.5<-results.prob.dropF.equiv  # type I error without dropping
results.biasF.fixed0.5<-results.biasF.equiv   # bias results without dropping

# fixed  alpha0=0.25
fixed.0.25<-paste0(path.stem,paste0("powerFixedmaxalpha0.25n",n.res,".RData"))
load(fixed.0.25)
results.prob.dropF.fixed0.25<-results.prob.dropF.wbord  # type I error without dropping
results.biasF.fixed0.25<-results.biasF.wbord   # bias results without dropping


# fixed max alpha0=0
results.prob.dropF.fixed0<-power.t.test(n=n.res,type = "one.sample",delta = c(0,.1,.2,.3,.4),sig.level = 0.025, alternative = "one.sided")$power


### Figure 2 

# extract alpha0 part
raten100<-res.100$alpha
#raten50<-res.50$alpha
#raten25<-res.25$alpha

# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6), 
                 percent=as.factor(rep(c(100,75,50,25), 5*6)),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4),1),
                 #Size=rep(c("100","050","025"),each=11*4*4),
                 rate=
                   c(raten100#,raten50,raten25
                    ))



# Figure 2 (by percent as columns)

library(lattice)
source("stripFunctionsv3.R")

tp<-trellis.par.get()
trellis.par.set(layout.heights = list(axis.xlab.padding=0), superpose.line=list(lty=c(1,4,3,2,5,6),
                                                                                lwd=2,
                                                                                col=1)) # default is 1
xyplot(rate~mu|percent,data=sims,groups=similarity,subscripts = TRUE,
       subset = sims$percent!="75", layout=c(3,1), as.table=TRUE, aspect = 1,

              key = simpleKey(c("SO-1", "SO-2", 
                       expression(paste(delta, "= 0.04")), expression(paste(delta, "= 0.08")), 
                       expression(paste(delta, "= 0.12")),expression(paste(delta, "= 0.16"))), 
      
                      lines=T,col=1, between=.5,
                       points=F,border=T, columns=6, space="top",
                       title="Similarity Measures",cex.title=1), 
       
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste(alpha[0],group("(",list(D[0],D[1]),")"))),
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.10)), 
                   x=list(at=c(0,.1,.2, .3, .4)),
                   tck=c(1,0),relation="same",alternating=c(1,1)), 
       strip=my.strip5aa,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x,y,subscripts=subscripts,groups=groups,lty=c(1,4,3,2,5,6), col.line=1, col=1,
                       type="b",pch=20,cex=1,lwd=2)
       }
)
trellis.par.set(tp) # default is 1



## Figure 3 (type I error rate and power)

# get results for typeI error rate
raten100<-res.100$typeI
# raten50<-res.50$typeI
# raten100<-res.25$typeI


# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6*2), 
                 percent=as.factor(rep(c(100,75,50,25), 5*6*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),6*1),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4*2),1),
                # Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100#,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA


# Figure 3 (by percent as columns; used in paper)

library(lattice)
source("stripFunctionsv3.R")
library(latticeExtra)

typeI<-list(as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="aSO1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="aSO2", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ1", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ2", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ3", "rate"],3)),
            as.character(round(sims[sims$discard.D1=="no" & sims$mu==0.0 & sims$similarity=="EQ4", "rate"],3)
              ))


typeI<-lapply(typeI,function(x) x[c(4,3,1)])


tp<-trellis.par.get()
trellis.par.set(layout.heights = list(axis.xlab.padding=0), superpose.line=list(lty=c(1,4,3,2,5,6),
                                                                                lwd=2,
                                                                                col=1)) 
xyplot(rate~mu|percent,data=sims,groups=similarity,subscripts = TRUE,
       subset = sims$percent!="75" & sims$discard.D1=="no" & sims$mu!=0.4, 
       layout=c(3,1), as.table=TRUE, aspect = 1.25,
       key = simpleKey(c("SO-1", "SO-2", 
                       expression(paste(delta, "= 0.04")), expression(paste(delta, "= 0.08")), 
                       expression(paste(delta, "= 0.12")),expression(paste(delta, "= 0.16"))), 
       
                       col=1,lines=T, between=.3, between.columns=0,
                       points=F,border=T, columns=6, space="top",
                       title="Similarity Measures",cex.title=1.08), 
       xlab = expression(paste(theta^"*")), ylab = "Power",
       
       scales=list(y=list(limits=c(0.0,1), at=seq(0.0,1,by=0.1)), 
                   x=list(at=c(0,.1,.2, .3),#, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ")),#,"0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.8), 
       strip=my.strip5aa,
       
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x,y,subscripts=subscripts,groups=groups,col=1, 
                      type="b",pch=20,cex=.65,lwd=2)
         panel.lines(x=c(0,.1,.2, .3), y=results.prob.dropF.fixed1[1:4], col="dark gray",pch=20,type="b",
                     lty=7,cex=.65,lwd=2)
         panel.lines(x=c(0,.1,.2, .3), y=results.prob.dropF.fixed0.5[1:4], col="dark gray",pch=20,type="b",
                     lty=8,cex=.65,lwd=2)
         panel.lines(x=c(0,.1,.2, .3), y=results.prob.dropF.fixed0[1:4], col="dark gray",pch=20,type="b",
                     lty=9,cex=.65,lwd=2)
         if(panel.number()==2){
           trellis.par.set(superpose.line=list(lty=c(7,8,9),lwd=2,col="dark gray")) 
            panel.key(c("1.0","0.5","0"), col=1, points=F,
                      border=F, columns=3, corner=c(0.8,0.025), between.columns=.25, between=.2,lines=T,
                   title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.9, cex=.8,font=1)
         }
         panel.key(unlist(lapply(typeI,function(x,num=panel.number()) x[num])), columns=1, between=0.5,
                   corner=c(.1,.9), cex=.75, points=F, border=F, font=1, lines=T,
                   col=1, title=c("Type I error rate"), cex.title=.9)
                }
)
trellis.par.set(tp) 




#### Figure 4 (bias or MSE, if desired)

# get results for bias
raten100<-res.100$bias # bias
#raten100<-res.100$bias^2 + res.100$sdF^2  # MSE
#raten50<-res.50$bias
#raten25<-res.25$bias


# create the whole data frame
sims<-data.frame(mu=rep(rep(c(0,.1,.2, .3, .4),each=4),6*2), 
                 percent=as.factor(rep(c(100,75,50,25), 5*6*2)),
                 
                 discard.D1=rep(rep(c("no", "yes"), each=20),6*1),
                 similarity=rep(rep(c("EQ1","EQ2","EQ3","EQ4","aSO1","aSO2"),each=5*4*2),1),
                 # Size=rep(c("100","050","025"),each=5*4*4*2),
                 
                 rate=
                   c(raten100#,raten50,raten25
                   ))

# We have not discarded 100% of the data, so set these to NA
sims$rate[(sims$discard.D1=="yes" & sims$percent=="100")]<-NA



## Figure 4 Bias (with percent of D as column). Used in paper

library(lattice)
library(latticeExtra)
source('stripFunctionsv3.R')

tp<-trellis.par.get()
trellis.par.set(layout.heights = list(axis.xlab.padding=0), superpose.line=list(lty=c(1,4,3,2,5,6),
                                                                                lwd=2,
                                                                                col=1)) 
xyplot(rate~mu|percent,data=sims,groups=similarity,subscripts = TRUE,
       subset = sims$percent!="75" & sims$discard.D1=="no", 
       layout=c(3,1), as.table=TRUE, aspect = 1.25,
       key = simpleKey(c("SO-1", "SO-2", 
                         expression(paste(delta, "= 0.04")), expression(paste(delta, "= 0.08")), 
                         expression(paste(delta, "= 0.12")),expression(paste(delta, "= 0.16"))), 
                       
                       col=1,lines=T, between=.5, between.column=.5,
                       points=F,border=T, columns=6, space="top",
                       title="Similarity Measures",cex.title=1), 
       xlab = expression(paste(theta^"*")), 
       ylab = expression(paste("Bias of ", theta^"*")),
       
       scales=list(y=list(limits=c(-0.10,.1), at=seq(-0.10,.1,by=0.02)), 
                   x=list(at=c(0,.1,.2, .3, .4), 
                          labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
                   tck=c(1,0),relation="same",alternating=c(1,1), cex=0.8), 
       strip=my.strip5aa,
       panel = function(x, y,subscripts=subscripts,groups) {
         panel.xyplot(x,y,subscripts=subscripts,groups=groups,col=1, lty=c(1,4,3,2,5,6), 
                      type="b",pch=20,cex=.65,lwd=2)
         panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col="dark gray", pch=20,type="b",lty=7,cex=.65,lwd=2)
         panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray", pch=20,type="b",lty=8,cex=.65,lwd=2)
         if(panel.number()==2){
           trellis.par.set(superpose.line=list(lty=c(7,8),lwd=2,col="dark gray")) 
           panel.key(c("1.0","0.5"), col=1,points=F, lines=T,
                     border=T, columns=2, corner=c(0.4,0.05), between=1, between.column=.5,
                     title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85,font=1)
         }
       }
)
trellis.par.set(tp) 


## Figure Bias SO 1-sided (with fixed alpha0 superimposed)

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="aSO1" ),  
#        layout=c(2,1), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.16,.11), at=seq(-.16,.11,by=0.02)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip6a,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          if(panel.number()==1) {
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.6,0.85),
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
#          }
#          panel.abline(h=0)
#        }
# )
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



## Figure Bias SO 2-sided (with fixed alpha0 superimposed)

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="aSO2" ),  
#        layout=c(2,1), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.1,.1), at=seq(-.1,.1,by=0.02)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip6a,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          if(panel.number()==1) {
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.6,0.85),
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
#          }
#          panel.abline(h=0)
#        }
# )
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1
# 


## Figure Bias Delta similarity (with fixed alpha0 superimposed)

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|discard.D1,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="EQ4" ),  
#        layout=c(2,1), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.1,.1), at=seq(-.1,.1,by=0.02)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip6a,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          if(panel.number()==1) {
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.6,0.85),
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.85, cex=.85)
#          }
#          panel.abline(h=0)
#        }
# )
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1

## Figure bias Delta region 0.04 and 0.10 (with fixed alpha0 superimposed) All on one figure

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="EQ1" | sims$similarity=="EQ4"),  
#        layout=c(2,2), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.06,.06), at=seq(-.06,.06,by=0.02)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip9c,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          if(panel.number()==1 | panel.number()==3) {
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.8,0.85), 
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.8, cex=.8)
#          }
#        }
# )
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1


## Figure bias SO measures (with fixed alpha0 superimposed) All on one figure

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|discard.D1*similarity,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="aSO1" | sims$similarity=="aSO2"),  
#        layout=c(2,2), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.16,.10), at=seq(-.16,.10,by=0.04)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip6b,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          if(panel.number()==1 | panel.number()==3) {
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#            panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.8,0.9), 
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.8, cex=.8)
#          }
#        }
# )
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1
# 

## Figure bias Delta region 0.04 and 0.10 , and SO-1 and 2-sided (with fixed alpha0 superimposed) All on one figure. No discard D1.

# library(lattice)
# library(latticeExtra)
# source("stripFunctionsv3.R")
# 
# trellis.par.set(layout.heights = list(axis.xlab.padding=0)) # default is 1
# xyplot(rate~mu|similarity,data=sims,groups=percent,subscripts = TRUE,
#        subset=(sims$similarity=="EQ1" | sims$similarity=="EQ4" | sims$similarity=="aSO1" | sims$similarity=="aSO2") & 
#          sims$discard.D1=="no",  
#        layout=c(2,2), as.table=TRUE,
#        key = simpleKey(c("100", "75", "50", "25"), col=c(1,4,3,2),points=F,border=T,
#                        columns=4, space="top",
#                        title="Percent of D used",cex.title=.85), 
#        xlab = expression(paste(theta^"*")), 
#        ylab = expression(paste("Bias of ", theta^"*")),
#        
#        scales=list(y=list(limits=c(-0.1,.1), at=seq(-.1,.1,by=0.04)), 
#                    x=list(at=c(0,.1,.2, .3, .4), 
#                           labels=c("   0.0","0.1","0.2 ","0.3  ","0.4")),
#                    tck=c(1,0),relation="same",alternating=c(1,1), cex=0.7), 
#        strip=my.strip9cc,
#        panel = function(x, y,subscripts=subscripts,groups) {
#          panel.xyplot(x, y,pch=20,col=c(1,4,3,2))
#          panel.superpose(x,y,subscripts=subscripts,groups=groups,col=1:4, 
#                          panel.groups=panel.xyplot, type="l")
#          panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed1, col=6,pch=20,type="b")
#          panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.5, col="dark gray",pch=20,type="b")
#          #panel.lines(x=c(0,.1,.2, .3, .4), y=results.biasF.fixed0.25, col="purple",pch=20,type="b")
#          
#          if(panel.number()==1) {
#            panel.key(c("1.0","0.5"), col=c(6,"dark gray"),points=F,border=T,
#                      columns=2, corner=c(0.8,0.85), 
#                      title=expression(paste("Fixed ",alpha[0], " value")),cex.title=.8, cex=.8)
#          }
#        }
# )
# trellis.par.set(layout.heights = list(axis.xlab.padding=1)) # default is 1



#### Table 1 SD comparison uses a different function 

### Average of ratios calculation

load("~/MDIC/simulations/output/SDResults/powerEQ1sideDeltaPonly0.12maxalpha1n100.RData")
usetwice<-results.sdi.equiv
usetwice.1<-lapply(usetwice, unlist, recursive=FALSE)

load("~/MDIC/simulations/output/SDResults/powerEQ1sideDeltaPonly0.12maxalpha1n100Ext.RData")
nousetwice<-results.sdi.equiv
nousetwice.1<-lapply(nousetwice, unlist, recursive=FALSE)


sdres16<-matrix(colMeans(mapply(`/`, nousetwice.1,usetwice.1)),nc=4,byrow=F)

res<-rbind(sdres04,sdres08,sdres12,sdres16,sdresSO1,sdresSO2)


library(htmlTable)
htmlTable(round(res,4))


################## older

# stem<-sd.summarize.no[,c(1:2,4)]
# stem$ratio<-txtRound(res,digits = 4)
# 
# 
# sd.summarize.yes<-sims[sims$twice.D1=="yes",]
# sd.summarize.no<-sims[sims$twice.D1=="no",]
# 
# 
# library(htmlTable)
# stem<-sd.summarize.no[,c(1:2,4)]
# stem$ratio<-txtRound(sd.summarize.no$rate/sd.summarize.yes$rate,digits = 4)
# 
# res<-reshape(stem, idvar=c("similarity","mu"),direction = "wide",timevar = c("percent"), v.names = "ratio")
# levels(res$similarity)<-c("Stoc Order 1-sided", "Stoc Order 2-sided", "Delta Sim 0.04", "Delta Sim 0.06",
#                           "Delta Sim 0.08", "Delta Sim 0.10") 
# 
# res.tab<-htmlTable(res, rnames=FALSE, 
#                    header=c("mu", "Similarity Measure", "100%", "75%", "50%", "25%"))
