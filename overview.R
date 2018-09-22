
# Arguments:
#
# discfun = either "wb" for Weibull using KS and Data, or "wbord" for Weibull using stochastic ordering
# ws = weibull scale
# wsh = weibull shape
# n0 = sample size for prior and data (n)
# power.null = null for power simulations
# two.sided = TRUE for two-sided discount function
# external = should alpha0 be determined using external data set of same distribution and size as current data?
#   Default is false, that is use percent of current data to determine alpha0
# mus = vector of true thetas of D for simulation
#       Note that mu0 = 0 (prior mean): D0 ~ N(0, 1)
#       Data ~ N(mus, 1)
# max_alpha = maximum fraction for alpha0
# percents = vector of percent of D to use for D1 
#
# Output: null.  
# Side effects: an RData file is written with numeric vectors of length=length(mus) as follows:
#
# power/type I error rate: results.prob.dropT.Wb, results.prob.dropF.Wb
#        dropT means that D1 is dropped from the LH after using it in alpha0
#        dropF means that D1 is not dropped
# alpha0 estimate: results.alpha.Wb
# SD estimate: results.SDT.Wb (D1 is dropped after using it in alpha0)
#    results.SDTF.Wb (D1 is not dropped)
# bias estimate: results.biasT.Wb, results.biasF.Wb


tryCatch(source("runSimulations.R"), finally=sink())