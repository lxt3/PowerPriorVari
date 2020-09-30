#################################### 1. Preliminary functions ##############################

#### Adapted from Haddad et al. (2017) Incorporation of stochastic engineering models as prior information in Bayesian medical device trials.  Journal of  Biopharmaceutical Statistics. 10: 1-15.
## See R package bayesDP

############################################################# Calculates posterior estimation for mu distribution# Given alpha_loss
############################################################# value and maximum strength of(N0_max) # prior if alpha_loss =1 #
mu_post_aug1 = function(mu, sigma2, N, mu0, sigma02, N0, alpha_loss, number_mcmc) {
  
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

mu_posterior = function(mu, sigma2, N, mu0, sigma02, N0, number_mcmc, D0, D, max_alpha ) 
{
  alpha_loss = Loss_function1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, number_mcmc = number_mcmc, D0,D)
  
  mu_posterior = mu_post_aug1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, #N0_max = N0_max, 
                              alpha_loss = min(alpha_loss$alpha_loss,max_alpha), 
                              number_mcmc = number_mcmc)
  
  return(list(alpha_loss = min(alpha_loss$alpha_loss,max_alpha), 
              mu_posterior = mu_posterior$mu_post,
              sigma2_posterior = mu_posterior$sigma2_post,
              effective_N0=mu_posterior$effective_N0, 
              mu_posterior_flate = alpha_loss$mu_post_flate, 
              mu_prior = alpha_loss$mu0, 
              mu = mu, N = N, mu0 = mu0, N0 = N0))
}

######## modified mu_posterior to input alpha_loss from a prior interim analysis
mu_posterior2 = function(mu, sigma2, N, mu0, sigma02, N0, number_mcmc, alpha_loss) 
{
  
  mu_posterior = mu_post_aug1(mu = mu, sigma2 = sigma2, N = N, mu0 = mu0, 
                              sigma02 = sigma02, N0 = N0, alpha_loss = alpha_loss, 
                              number_mcmc = number_mcmc)
  
  return(list( mu_posterior = mu_posterior$mu_post,
               sigma2_posterior = mu_posterior$sigma2_post,
               effective_N0=mu_posterior$effective_N0, 
               mu = mu, N = N, mu0 = mu0, N0 = N0))  
}


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
  #print(obsmeandiff)
  ########## test of model vs real###########
  
  if(discfun=="wb"){ # KS measure of similarity
    
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
    
  }
  else if(discfun=="wbord"){ # stochastic ordering similarity
    p_test1<-mean(meandiff>=0)#/(N0-N+1)  # if current mean better than prior, higher p
    
    if(two.sided){
      # two-sided stochastic ordering
      if(p_test1 > .5) p_test1<-1-p_test1
    } 
  }

  else { # delta similarity margin measure
    if((obsmeandiff>=-delta) && (obsmeandiff<=delta))
    {
      # post prob true diff is between -delta and delta
      p_test1<-mean(((meandiff>-delta) & (meandiff<delta))) # how many mean diffs fall within equiv interval
    } 
    else {
      p_test1<-0
    }
  }
  print(p_test1)
  alpha_loss <-if(post.prob.only) p_test1 else {alpha_lossf(p_test1) }
  
  
  return(list(alpha_loss = alpha_loss,  mu_post_flate = mu_post_flate,  mu0 = mu_post_flate0))
  
}


#################################### 2. Compute Posterior probability of H1 for given data sets ##############################

n0<-300 
sd<-21.3

mu0<- 23.6 # true prior data mean 
mu<- 22.0 # true current data mean

# generate or set D0 
D0<-rnorm(n0, mean=mu0, sd=sd)
D0<-scale(D0)
D0<-as.numeric(sd*D0 + mu0)
# obtain sufficient statistics:
sd0<-sd(D0)
mean0<-mean(D0)

# Samples sizes
delta<-1 # used with equivalence similarity measure
         # could also use: mean0-qnorm(.84)*sd0 
power.n<-power.t.test(power=.80,sd=sd0, sig.level = 0.025, alternative = "one.sided", 
                      type = "one.sample",delta=3)$n
max_alpha<- .5 # could also use: 1-delta/abs(mean0) #.5
n<-round(power.n)-max_alpha*n0 # current data sample size minus maximum allowed borrowing from D0

# generate D from  mu and sd0
set.seed(1) # used for example in paper
D<-rnorm(mean=mu, n=n, sd=sd0)


# num mcmc iterations
nmcmc<-100000

null<-20  # null value of mu in H0
prob.H1<-.975 # threshold for post prob of H1


# percent of D to make up D1
percent <-.5

# set D1 = x% of D
D1<- D[1:(percent*n)]


#### Define discount function:
discfun<- "equiv" #"wbord"  #"wb"  # what is the general discount fcn: 
                                   # "equiv" = similarity region disc fun
                                   # "wb" = KS discount fcn (old, not used anymore)
                                   # "wbord" = stochastic ordering (as per Haddad et al. 2017)
two.sided<-F#T  # two-sided discount function or not (equiv is always one-sided, not matter what you put here)

post.prob.only<-T # used with all similarity measures except KS ("wb"); 
                  # T = identity discount fcn
delta<-delta # used with equivalence similarity measure

########### Used to define a discount function other than identity (i.e., post.prob.only=FALSE)
if(discfun=="wb"  ){
  if(two.sided){
    ws=.9; wsh=12;  # if weibull_scale=99 that represents pvalue equals weight
  } else{
    ws=.9; wsh=20;
  }
}

if(discfun=="wbord"){
  if(two.sided){
    ws=.4; wsh=1.5; 
  } else{
    ws=.65; wsh=3; 
  }
}

if(discfun=="equiv" ){
  ws=.65; wsh=3; 
}
########################################################################


## alpha_lossf as a functional 
require(pryr)
alpha_lossf<-partial(pweibull, shape=wsh, scale=ws)



# estimate alpha0 using D0 and D1 

fit<-mu_posterior(mu      = mean(D1),
                  sigma2   = var(D1),
                  N       = length(D1), 
                  mu0     = mean(D0),
                  sigma02  = var(D0),
                  N0      = length(D0),
                  number_mcmc=nmcmc,
                  D0=D0, D=D1,
                  max_alpha=max_alpha
)
  
alpha_use<- fit$alpha_loss  


# use alpha0 from above to estimate posterior

D2<-D

fit<-mu_posterior2(mu      = mean(D2),
                   sigma2   = var(D2),
                   N       = length(D2), 
                   mu0     = mean(D0),
                   sigma02  = var(D0),
                   N0      = length(D0),
                   number_mcmc=nmcmc,
                   alpha_loss=alpha_use
)

print(mean(fit$mu_posterior > null))  # posterior probability of H1


# Borrowing 0%
fit0<-mu_posterior2(mu      = mean(D2),
                   sigma2   = var(D2),
                   N       = length(D2), 
                   mu0     = mean(D0),
                   sigma02  = var(D0),
                   N0      = length(D0),
                   number_mcmc=nmcmc,
                   alpha_loss=0
)


print(mean(fit0$mu_posterior > null)) # posterior probability of H1

# print out other results, if desired
# print(mean(D0))
# print(mean(D))
# print(mean(D1))
# 
# print(mean(fit$mu_posterior))
# print(mean(fit0$mu_posterior))
