
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
  eWait = rep(0, nTimeStep); # es vector for "wait"
  eQuit = 0; # es vector for "quit"
  LL = 0
  
  # loop over trials and skip the last trial
  for(tIdx in 1 : (nTrial - 1)){
    t = 1
    nTimePoint = getNTimePoint(trialEarnings[tIdx],
                               timeWaited[tIdx], stepDuration)
    # loop over time points 
    while(t <= nTimePoint){
      # update lik
      if(tau* Qwait[t] - log(sum(exp(tau * Qwait[t]) + exp(tau * Qquit))) > 0){
        browser()
      }
      LL = LL + tau* Qwait[t] - log(sum(exp(tau * Qwait[t]) + exp(tau * Qquit)))
      
      # determine action, nextReward, stepGap, nextT, nextAction
      action = getAction(trialEarnings[tIdx], t, nTimePoint)
      nextReward = getNextReward(trialEarnings[tIdx], t, nTimePoint)
      stepGap = getStepGap(trialEarnings[tIdx], t, nTimePoint, stepDuration)
      actionNextTrial = ifelse(timeWaited[tIdx + 1] > 0, 'wait', 'quit')
      nextAction = getNextAction(trialEarnings[tIdx], t, nTimePoint, actionNextTrial)
      nextT = ifelse(t >= nTimePoint, 1, t + 1)
      
      # update eWait and eQuit
      junk = rep(0, nTimeStep)
      junk[t] = 1
      eWait =  pmin(gamma^stepGap * lambda * eWait + junk * c(action == "wait"), 1)
      eQuit = min(gamma ^stepGap * lambda * eQuit + c(action == "quit"), 1)
      
      # update Qwait and Qquit
      delta = nextReward +
        gamma^(stepGap) * ifelse(nextAction == 'wait',  Qwait[nextT], Qquit)-
        ifelse(action == 'wait', Qwait[t], Qquit)
      Qwait = Qwait + phi * delta * eWait
      Qquit = Qquit + phi * delta * eQuit
      
      # update T
      if(nextT == 1){
        break
      }else{
        t = nextT
      }
      

    }# end of one trial
  }# end of all trials
  negLL = -LL
  return(negLL)
} #end of the function
