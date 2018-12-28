
##############################s
negLLAction  = function(x, otherPara, cond, wIni, trialEarnings, timeWaited){
  # parse learning para
  para = x
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = 1
  
  # task para
  source('subFxs/taskFxs.R')
  source("subFxs/wtwSettings.R")
  nStepDuration = 0.5
  
  # parse otherPara
  tMax= otherPara[['tMax']]
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  nTrial = length(trialEarnings)
  nTimeStep = tMax / stepDuration
  
  # initialize action values, eligibility traces and lik
  Qwait = rep(wIni, nTimeStep) 
  Qquit = wIni * gamma ^(iti / stepDuration)
  LL = 0
  
  # need to skip 
  for(tIdx in 1 : nTrial){
    # determine nTimePoint
    waitDuration = timeWaited[tIdx]
    if(trialEarnings[tIdx] > 0){
      nTimePoint = ceiling(waitDuration / stepDuration)
    }else{
      nTimePoint = ceiling(waitDuration / stepDuration) + 1
    }
    # loop over time points 
    
    for(t in 1 : nTimePoint){
      # update lik
      if(trialEarnings[tIdx] == 0 & t == nTimePoint){
        LL = LL + tau* Qquit - log(sum(exp(tau * Qwait[t]) + exp(tau * Qquit)))
      }else{
        LL = LL + tau* Qwait[t] - log(sum(exp(tau * Qwait[t]) + exp(tau * Qquit)))
      }
    }
    # update Qwait and Qquit 
    if(trialEarnings[tIdx] > 0){
      trialReward = tokenValue
      Qwait[1 : t] = (1 - phi) * Qwait[1 : t] + phi * trialReward * gamma ^ rev((0 : (t - 1 )))
    }else{
      nextWaitRateHat =  1 / sum(1  + exp((Qquit - Qwait[1])* tau))
      trialReward = nextWaitRateHat * Qwait[1] * gamma ^(iti / stepDuration) +
        (1 - nextWaitRateHat) * Qquit * gamma ^(iti / stepDuration)
      
      Qquit =  (1 - phi) * Qquit + phi *  trialReward
      if(t > 1){
        Qwait[1 : (t - 1)] = (1 - phi) * Qwait[1 : (t - 1)] +
          phi * trialReward * gamma ^ rev((1 : (t - 1 )))
      }
      
    }# end of one trial
  }# end of all trials
  negLL = -LL
  return(negLL)
} #end of the function
