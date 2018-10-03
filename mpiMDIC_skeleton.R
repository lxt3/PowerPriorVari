suppressMessages(library(pbdMPI, quietly = TRUE))

# Use mpi in modifiedMDICprogram.R

init()


rank<-comm.rank()
.comm.size<-comm.size()
nsim<-100


comm.set.seed(2, comm=0, diff=TRUE)


## divide simulations into np independent sets

rank.nsim<-round(nsim/.comm.size)
res<-alpha<-vector(length=rank.nsim)
n<-25
mu<-0

for(i in 1:rank.nsim){
  
  # generate ith D from these parameter configs
  D<-rnorm(mean=mu, n=n, sd=1)
  
  # get some results using D for this rank
  res[i]<-sum(D)
  alpha[i]<-1
}

# results per rank look like: res2[1:nsim.rank], res1, sigmaD1, sigmaD, sd.2, sd.1, bias1, bias2

# get sums per rank

## reduce the results by getting the means
 res.reduce<-reduce(res, op="sum" , rank.dest = 0)
 mean.res<-sum(res.reduce)/nsim #
 
 alpha.reduce<-reduce(alpha, op="sum" , rank.dest = 0)
 mean.alpha<-sum(alpha.reduce)/nsim #
 

# comm.cat to sink output

#comm.print(res, all.rank = TRUE)
comm.cat(paste("prob: ",mean.res,"\n"), all.rank = FALSE)
comm.cat(paste("alpha0: ", mean.alpha,"\n"), all.rank = FALSE)

finalize()