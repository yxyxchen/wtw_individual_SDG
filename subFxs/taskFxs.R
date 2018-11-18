###3

drawSample = function(distrib){
  # % generates a sample from next quantile of the designated distribution
  # % timing parameters are specified within this function

  k = pareto[['k']]
  mu = pareto[['mu']]
  sigma = pareto[['sigma']]
  
  if(distrib == 'unif20') sample = runif(1, min = 0, max = tMaxs[1])
  else sample = mu + sigma * (runif(1) ^ (-k) - 1) / k
  
  return(sample)
}


