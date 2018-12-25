
####################################################################
##this function returns the number of points to make decisions 
getNTimePoint = function(trialEarning, waitDuration, stepDuration){
  # at the end of the block, no decision needed after the block ends
  if(tIdx >= nTrial){
    nTimePoint = ceiling(waitDuration / stepDuration)
  }else{
    if(trialEarning > 0){
      nTimePoint = ceiling(waitDuration / stepDuration)
    }else{
      nTimePoint = ceiling(waitDuration / stepDuration) + 1
    }
  }
  return(nTimePoint)
}


####################################################################
getAction = function(trialEarning, tIdx, nTrial, t, nTimePoint){
  if(tIdx >= nTrial || trialEarning > 0){
    nTimePoint = ceiling(waitDuration / stepDuration)
  }else{
    if(trialEarning > 0){
      nTimePoint = ceiling(waitDuration / stepDuration)
    }else{
      nTimePoint = ceiling(waitDuration / stepDuration) + 1
    }
  }
  return(nTimePoint)
}