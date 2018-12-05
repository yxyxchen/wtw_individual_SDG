# this scripts contains some supporting functions for runing the simulation
# and transfer vaWaits and vaQuits
drawSample = function(cond){
  # % generates a sample from next quantile of the designated distribution
  # % timing parameters are specified within this function

  k = pareto[['k']]
  mu = pareto[['mu']]
  sigma = pareto[['sigma']]
  
  if(cond == 'HP') sample = runif(1, min = 0, max = tMaxs[1])
  else{
    sample = min(mu + sigma * (runif(1) ^ (-k) - 1) / k, tMaxs[2])
    
    }
  return(sample)
}

transVaWaits = function(vaWaits, endTick = ncol(vaWaits)){
  for(i in 2 : endTick){
    if(sum(is.na(vaWaits[,i])) > 0){
      vaWaits[is.na(vaWaits[,i]),i]  = vaWaits[is.na(vaWaits[,i]),i-1] 
    }
  }
  return(vaWaits)
}

transVaQuits = function(vaQuits, endTick = ncol(vaQuits)){
  for(i in 1 : endTick){
    if(sum(is.na(vaQuits[,i])) > 0){
      vaQuits[is.na(vaQuits[,i]),i] = vaQuits[match(NA,vaQuits[,i] ) - 1,i]
    }
  }
  return(vaQuits)
}








