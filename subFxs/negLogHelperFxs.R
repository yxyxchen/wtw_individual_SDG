
####################################################################
##this function returns the number of points to make decisions 
getNTimePoint = function(trialEarning, waitDuration, stepDuration){
  # at the end of the block, no decision needed after the block ends
  if(trialEarning > 0){
    nTimePoint = ceiling(waitDuration / stepDuration)
  }else{
    nTimePoint = ceiling(waitDuration / stepDuration) + 1
  }
  return(nTimePoint)
}

####################################################################
getAction = function(trialEarning, t, nTimePoint){
 # if quits and it is the last quitting decision
 if(trialEarning == 0 && t == nTimePoint){
   action = "quit"
 }else{
   action = 'wait'
 }
  return(action)
}

####################################################################
getNextReward = function(trialEarning, t, nTimePoint){
  # if at the end of the rewarding trial
  if(trialEarning > 0 && t == nTimePoint){
    nextReward = tokenValue
  }else{
    nextReward = 0
  }
  return(nextReward)
}

####################################################################
getStepGap = function(trialEarning, t, nTimePoint, stepDuration){
  if(t < nTimePoint){
    stepGap = 1
  }else{
    stepGap = ifelse(trialEarning > 0, iti / stepDuration + 1, iti / stepDuration)
  }
  return(stepGap)
}

####################################################################
getNextAction = function(trialEarning, t, nTimePoint, actionNextTrial){
  if(t < (nTimePoint - 1)){
    nextAction = 'wait'
  }else if(t == (nTimePoint - 1)){
    nextAction = ifelse(trialEarning >0, 'wait', 'quit')
  }else{
    nextAction = actionNextTrial
  }
}



