
############################################################# Calculates posterior estimation for mu distribution# Given alpha_loss
############################################################# value and maximum strength of(N0_max) # prior if alpha_loss =1 #
mu_post_aug1 = function(mu, sigma2, N, mu0, sigma02, N0, N0_max, alpha_loss, number_mcmc) {
  
  effective_N0 = N0 * alpha_loss # modified from orignal TH program
  
  sigma2_post = 1/rgamma(number_mcmc, (N - 1)/2, ((N - 1) * sigma2)/2)      # Posterior variance new data
  sigma2_post0 =  1/rgamma(number_mcmc, (N0 - 1)/2, ((N0 - 1) * sigma02)/2)  # Posterior variance Historical data
  mu1 = (sigma2_post0 * N * mu + sigma2_post * effective_N0 * mu0)/(N *       #Posterior mean estimation
                                                                      sigma2_post0 + sigma2_post * effective_N0)
  var_mu = (sigma2_post * sigma2_post0)/(N * sigma2_post0 + sigma2_post * 
                                           effective_N0)                                                       #Standard error of posterior
  mu_post = rnorm(number_mcmc, mu1, sqrt(var_mu))                          #Posterior samples of mean
  return(list(mu_post=mu_post,sigma2_post=sigma2_post,effective_N0=effective_N0))
}

###################Combines the loss function and posterior estimation into one function
mu_posterior = function(mu, sigma2, N, mu0, sigma02, N0, N0_max, number_mcmc, D0, D ) 
{
  alpha_loss = Loss_function1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, number_mcmc = number_mcmc, D0,D)
  
  mu_posterior = mu_post_aug1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, N0_max = N0_max, alpha_loss = alpha_loss$alpha_loss, 
                              number_mcmc = number_mcmc)
  
  return(list(alpha_loss = alpha_loss$alpha_loss, 
              mu_posterior = mu_posterior$mu_post,
              sigma2_posterior = mu_posterior$sigma2_post,
              effective_N0=mu_posterior$effective_N0, 
              mu_posterior_flate = alpha_loss$mu_post_flate, 
              mu_prior = alpha_loss$mu0, 
              mu = mu, N = N, mu0 = mu0, N0 = N0, N0_max = N0_max))
}

######## modified mu_posterior to input alpha_loss from a prior interim analysis
mu_posterior2 = function(mu, sigma2, N, mu0, sigma02, N0, N0_max, number_mcmc, alpha_loss) 
{
  
  mu_posterior = mu_post_aug1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, N0_max = N0_max, alpha_loss = alpha_loss, 
                              number_mcmc = number_mcmc)
  
  return(list( mu_posterior = mu_posterior$mu_post,
               sigma2_posterior = mu_posterior$sigma2_post,
               effective_N0=mu_posterior$effective_N0, 
               mu = mu, N = N, mu0 = mu0, N0 = N0, N0_max = N0_max))  
}


################# alpha_lossf as a functional #####################

require(pryr)
alpha_lossf<-partial(pweibull, shape=wsh, scale=ws)


##################### Discount function #############
# change function for loss based on "discfun", defined globally

Loss_function1 <-function(mu, sigma2, N, mu0, sigma02, N0, number_mcmc, D0, D) {
  
  ########## mu for using Flat prior###########
  sigma2_post_flate = 1/rgamma(number_mcmc, (N - 1)/2, ((N - 1) * sigma2)/2)  
  mu_post_flate = rnorm(number_mcmc, mu, (sigma2_post_flate/((N - 1) + 
                                                               1))^0.5)  
  ########## prior model###########
  sigma2_post_flate0 = 1/rgamma(number_mcmc, (N0 - 1)/2, ((N0 - 1) * sigma02)/2)  
  mu_post_flate0 = rnorm(number_mcmc, mu0, (sigma2_post_flate0/((N0 - 
                                                                   1) + 1))^0.5)  
  meandiff<-(mu_post_flate - mu_post_flate0)
  obsmeandiff<-mu-mu0
  
  ########## test of model vs real###########
  if(ws==99){  # ws is defined in global environment
    alpha_loss=p_test1
  }else{
    
    if(discfun=="wb"){
      
      if(two.sided){
        # two-sided KS measure
        stat<-as.numeric(ks.test(D0,D)$statistic) # uses actual data D0 and D
      }
      else{
        # one-sided KS
        stat<-as.numeric(ks.test(D0,D, alternative="less")$statistic)
      }
      
      stat<-ifelse(stat>0,stat,0)
      p_test1<-1-stat 
      alpha_loss <-alpha_lossf(p_test1)
      
    }
    else if(discfun=="wbord"){
      p_test1<-mean(meandiff>=0)#/(N0-N+1)  # if current mean better than prior, higher p
      
      if(!two.sided){
        # one-sided stochastic ordering
        alpha_loss <-alpha_lossf(p_test1)
      } 
      else {
        # two-sided stochastic ordering
        alpha_loss <- ifelse(p_test1<=.5, alpha_lossf(p_test1), alpha_lossf(1-p_test1))
      }
    }
    else { # new similarity function trying out
      if((obsmeandiff>=-delta) && (obsmeandiff<=delta))
        {
          # post prob true diff is between -delta and delta
          p_test1<-mean(((meandiff>-delta) & (meandiff<delta))) # how many mean diffs fall within equiv interval
      } 
      else {
        p_test1<-0
      }
      alpha_loss <-if(post.prob.only) p_test1 else {alpha_lossf(p_test1) }#p_test1#
    }
    
  }
  
  return(list(alpha_loss = alpha_loss,  mu_post_flate = mu_post_flate,  mu0 = mu_post_flate0))
  
}


FUN<-function(jid, nsim, n, mu, percent,null,prob.H1,nmcmc,max_alpha,D0)
{

  rank.nsim<-round(nsim/length(jid))
  res1<-res2<-bias1<-bias2<-alpha<-sd.1<-sd.2<-phat<-sigmaD<-sigmaD1<-vector(length=rank.nsim)
  
  
for(i in 1:rank.nsim){
  
  if((i/1000)%in%c(1:20)) print(i)
  
  # generate ith D from this mu and scale to give it sd=1
  D<-rnorm(mean=mu, n=n, sd=1)
  D<-D/sd(D)
  
  # set D1 = x% of D
  D1<- D[1:(percent*n)]
  
  
  # estimate alpha0 using D0 and D1
  
  if(external){
    D1<-rnorm(mu, n=percent*n, sd=1)
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
  
  
  
  # drop D1 (if percent<1)
  if(percent<1) {
    
    D2 <- D[-(1:(percent*n))] 
    
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
  } else {
    
    res2[i]<-res1[i]
    sigmaD1[i]<-sigmaD[i]
    sd.2[i]<-sd.1[i]
    bias2[i]<-bias1[i]
    
  }
  
}
  list(res1=res1,alpha=alpha,res2=res2,bias1=bias1,bias2=bias2,sd.1=sd.1,sd.2=sd.2)
}