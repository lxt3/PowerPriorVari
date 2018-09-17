# One-arm trial (OPC) example

# reset vectos
res1<-res2<-bias1<-bias2<-alpha<-sd.1<-sd.2<-phat<-sigmaD<-sigmaD1<-vector(length=nsim)


# Generate nsim current data sets and compute posterior inference using prior D0
for(i in 1:nsim){
  
  if((i/1000)%in%c(1:20)) print(i)
  
  # generate ith D from this mu
  D<-rnorm(mean=mu, n=n)
  
  # set D1 = x% of D
  D1<- D[1:(percent*n)]
  
   
  # estimate alpha0 using D0 and D1
  
  if(external){
    D1<-rnorm(mu, n=percent*n)
    fit<-mu_posterior(mu      = mean(D1),
                      sigma2   = var(D1),
                      N       = length(D1), 
                      mu0     = mean(D0),
                      sigma02  = var(D0),
                      N0      = length(D0),
                      N0_max=length(D0)*max_alpha, 
                      number_mcmc=nmcmc,
                      D0=D0, D=D1
    )
  }
  else{
  fit<-mu_posterior(mu      = mean(D1),
               sigma2   = var(D1),
               N       = length(D1), 
               mu0     = mean(D0),
               sigma02  = var(D0),
               N0      = length(D0),
               N0_max=length(D0)*max_alpha, 
               number_mcmc=nmcmc,
               D0=D0, D=D1
  )
  }

 alpha_use<- alpha[i]<-ifelse(fit$alpha_loss>max_alpha, max_alpha, fit$alpha_loss)  

  
  # use alpha0 from above to estimate posterior
  
  # first no drop
  D2<-D
  
  fit<-mu_posterior2(mu      = mean(D2),
                    sigma2   = var(D2),
                    N       = length(D2), 
                    mu0     = mean(D0),
                    sigma02  = var(D0),
                    N0      = length(D0),
                    N0_max=length(D0)*max_alpha, 
                    number_mcmc=nmcmc,
                    alpha_loss=alpha_use
  )
  
  res1[i]<-mean(fit$mu_posterior > null)>=prob.H1  # prob H1 (or prob reject H0)

  sd.1[i]<-sd(fit$mu_posterior)
  sigmaD[i]<-mean(fit$sigma2_posterior)
  
  bias1[i]<-mean(fit$mu_posterior)-mu



  # drop D1
  if(percent<1) D2 <- D[-(1:(percent*n))] else D2<-D
  
  # use alpha0 from above to estimate posterior
  fit<-mu_posterior2(mu      = mean(D2),
                     sigma2   = var(D2),
                     N       = length(D2), 
                     mu0     = mean(D0),
                     sigma02  = var(D0),
                     N0      = length(D0),
                     N0_max=length(D0)*max_alpha, 
                     number_mcmc=nmcmc,
                     alpha_loss=alpha_use
  )
  
  res2[i]<-mean(fit$mu_posterior > null)>=prob.H1
  sigmaD1[i]<-mean(fit$sigma2_posterior)
  
  
  sd.2[i]<-sd(fit$mu_posterior)
  
  bias2[i]<-mean(fit$mu_posterior)-mu
  
  
}

# print to sink output
cat("percent = \n")
print(percent)
cat("mu = \n")
print(mu)
cat("n = \n")
print(n)
cat("null = \n")
print(null)
cat("external = \n")
print(external)

cat("No dropping\n")

print(mean(res1))
print(mean(sd.1))

cat("bias: No dropping\n")
print(mean(bias1))


cat("alpha\n")
print(mean(alpha))
#print(mean(sd.1))

cat("With dropping\n")

print(mean(res2))
print(mean(sd.2))

cat("bias: Dropping\n")
print(mean(bias2))
