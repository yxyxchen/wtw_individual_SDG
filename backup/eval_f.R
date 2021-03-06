eval_f_wait = function(x, otherPara, cond, wIni, rewardDelays, trueTimeWaited){
  # set.seed(123)
  para = x
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = 1
  
  # task para
  source('subFxs/taskFxs.R')
  source("subFxs/wtwSettings.R")
  
  # read otherPara
  tMax= otherPara[['tMax']]
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  nTimeStep = tMax / stepDuration
  nTrial = length(rewardDelays)
  
  
  ########### simulation repeatedly ############
  # initialize action value, eligibility trace and stat
  # exp(-r * stepDuration) = gamma
  Qwait = rep(wIni, nTimeStep) # Q(si, ai = wait), any i
  Qquit = wIni * gamma ^(iti / stepDuration)
  eWait = rep(0, nTimeStep); # es vector for "wait"
  eQuit = 0; # es vector for "quit"
  
  
  # initialize stepGap
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0,  nTrial + 1)
  timeWaited = rep(0,  nTrial + 1)
  
  
  # initialize xs and action
  xs = 1 
  waitRate = 1 / sum(1  + exp((Qquit - Qwait[1])* tau))
  action = ifelse(runif(1) < waitRate, 'wait', 'quit')
  
  # loop until time runs out
  for(tIdx in 1 : nTrial) {
    # sample rewardDelay
    rewardDelay = rewardDelays[tIdx]
    # no time limiting 
    for(t in 1 : nTimeStep){
      tryCatch({
        # next reward 
        # determine whether reward occurs in the step t
        # the previous code is wrong, since rewards happens on 16s seconds woudldn't be counted 
        rewardOccur = (rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t])
        
        # if rewarded and wait, 5; otherwise, 0
        getReward = (action == 'wait' && rewardOccur);
        nextReward = ifelse(getReward, tokenValue, 0) 
        
        # dertime next state
        # go to the terminate state if at the final step or quit or reward arrives
        trialGoOn= (action == 'wait' && !rewardOccur && t < nTimeStep)
        if(is.na(trialGoOn) || is.nan(trialGoOn) ) browser()
        if(trialGoOn){
          nextXs = xs + 1
        }else{
          nextXs  = 1
        }
        
        # choose next action given next state 
        
        nextWaitRate = 1 / sum(1 + exp((Qquit - Qwait[nextXs])* tau)) # better in this form, otherwise might inf / inf
        nextAction = ifelse(runif(1) < nextWaitRate, 'wait', 'quit')
        
        # update eligilibity trace
        # here stepGap meatured between At and At-1
        junk = rep(0, nTimeStep)
        junk[xs] = 1
        eWait =  gamma^stepGap * lambda * eWait + junk * c(action == "wait")
        eQuit = gamma ^stepGap * lambda * eQuit + c(action == "quit") 
        
        # update stepGap
        stepGap = ifelse(trialGoOn, 1, ifelse(action == 'quit', iti / stepDuration, iti / stepDuration + 1))
        
        # update action value of quit and wait
        # here stepGap meatured between At and At+1
        delta = nextReward + gamma^(stepGap) * ifelse(nextAction == 'wait',  Qwait[nextXs], Qquit)-ifelse(action == 'wait', Qwait[xs], Qquit)
        # anything wrong with the delta here?
        Qwait = Qwait + phi * delta * eWait
        Qquit = Qquit + phi * delta * eQuit
        
        
        # update xs and action
        xs = nextXs
        action = nextAction
        
        # break the trial didn't continue
        # return output
        if(!trialGoOn){
          trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
          # if quit, quit at t, if wait, wait until t+1
          timeWaited[tIdx] = ifelse(getReward, rewardDelay, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
          break
        }
      }, error = function(e) {
        browser()
      })
    }  # one trial end
  } # simulation end
  nTrial = length(rewardDelays)
  LL = mean((timeWaited[1 : nTrial] - trueTimeWaited)^2)
  return(LL)
} #end of the function

##############################
##############################s
eval_f_dv = function(x, otherPara, cond, wIni, rewardDelays, trueDvs){
  # set.seed(123)
  x = para
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = 1
  
  # task para
  source('subFxs/taskFxs.R')
  source("subFxs/wtwSettings.R")
  
  # read otherPara
  tMax= otherPara[['tMax']]
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  nTimeStep = tMax / stepDuration
  nTrial = length(rewardDelays)
  
  
  ########### simulation repeatedly ############
  # initialize action value, eligibility trace and stat
  # exp(-r * stepDuration) = gamma
  Qwait = rep(wIni, nTimeStep) # Q(si, ai = wait), any i
  Qquit = wIni * gamma ^(iti / stepDuration)
  eWait = rep(0, nTimeStep); # es vector for "wait"
  eQuit = 0; # es vector for "quit"
  
  # recordings of vaWait and vaQuit
  vaWaits = matrix(NA, nTimeStep, nTrial + 1);
  vaWaits[,1] = wIni;
  vaQuits = matrix(NA, nTimeStep,  nTrial + 1);
  vaQuits[1,1] = wIni* gamma ^(iti / stepDuration);
  
  # initialize stepGap
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0,  nTrial + 1)
  timeWaited = rep(0,  nTrial + 1)
  
  
  # initialize xs and action
  xs = 1 
  waitRate = 1 / sum(1  + exp((Qquit - Qwait[1])* tau))
  action = ifelse(runif(1) < waitRate, 'wait', 'quit')
  
  # loop until time runs out
  for(tIdx in 1 : nTrial) {
    # sample rewardDelay
    rewardDelay = rewardDelays[tIdx]
    # no time limiting 
    for(t in 1 : nTimeStep){
      # next reward 
      # determine whether reward occurs in the step t
      # the previous code is wrong, since rewards happens on 16s seconds woudldn't be counted 
      rewardOccur = (rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t])
      
      # if rewarded and wait, 5; otherwise, 0
      getReward = (action == 'wait' && rewardOccur);
      nextReward = ifelse(getReward, tokenValue, 0) 
      
      # dertime next state
      # go to the terminate state if at the final step or quit or reward arrives
      trialGoOn= (action == 'wait' && !rewardOccur && t < nTimeStep)
      if(trialGoOn){
        nextXs = xs + 1
      }else{
        nextXs  = 1
      }
      
      # choose next action given next state 
      
      nextWaitRate = 1 / sum(1 + exp((Qquit - Qwait[nextXs])* tau)) # better in this form, otherwise might inf / inf
      nextAction = ifelse(runif(1) < nextWaitRate, 'wait', 'quit')
      
      # update eligilibity trace
      # here stepGap meatured between At and At-1
      junk = rep(0, nTimeStep)
      junk[xs] = 1
      eWait =  gamma^stepGap * lambda * eWait + junk * c(action == "wait")
      eQuit = gamma ^stepGap * lambda * eQuit + c(action == "quit") 
      
      # update stepGap
      stepGap = ifelse(trialGoOn, 1, ifelse(action == 'quit', iti / stepDuration, iti / stepDuration + 1))
      
      # update action value of quit and wait
      # here stepGap meatured between At and At+1
      delta = nextReward + gamma^(stepGap) * ifelse(nextAction == 'wait',  Qwait[nextXs], Qquit)-ifelse(action == 'wait', Qwait[xs], Qquit)
      # anything wrong with the delta here?
      Qwait = Qwait + phi * delta * eWait
      Qquit = Qquit + phi * delta * eQuit
      
      # save changes in vaWaits, so vaWaited updated at t is used for t+1
      # Qquit has not state 
      if(!trialGoOn){
        vaQuits[1, tIdx + 1] = Qquit;
        if(t < nTimeStep){
          vaQuits[t + 1, tIdx ] = Qquit;
        }
      }else{
        vaQuits[t + 1, tIdx] = Qquit;
      }    
      
      if(!trialGoOn){
        vaWaits[1 : xs, tIdx + 1] = Qwait[1 : xs];
      }
      # update xs and action
      xs = nextXs
      action = nextAction
      
      # break the trial didn't continue
      # return output
      if(!trialGoOn){
        trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
        # if quit, quit at t, if wait, wait until t+1
        timeWaited[tIdx] = ifelse(getReward, rewardDelay, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
        break
      }
    }  # one trial end
  } # simulation end
  source('subFxs/taskFxs.R')
  nTrial = length(rewardDelays)
  vaWaits = transVaWaits(vaWaits[,1 : nTrial])
  vaQuits = transVaQuits(vaQuits[,1 : nTrial])
  dvs = vaWaits - vaQuits;
  LL = mean((dvs - trueDvs)^2)
  return(LL)
} #end of the function

##############################
# eval_f_action
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
  nTrial = length(rewardDelays)
  
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
      LL = LL + tau* Qwait[t] - log(exp(sum(tau * (Qquit + Qwait[t] ))))
      
      # determine action, nextReward, stepGap, nextT, nextAction
      action = getAction(trialEarnings[tIdx], t, nTimePoint)
      nextReward = getNextReward(trialEarnings[tIdx], t, nTimePoint)
      stepGap = getStepGap(trialEarnings[tIdx], t, nTimePoint, stepDuration)
      actionNextTrial = ifelse(timeWaited[tIdx + 1] > 0, 'wait', 'quit')
      nextAction = getNextAction(trialEarnings[tIdx], t, nTimePoint, actionNextTrial)
      nextT = ifelse(t >= nTimePoint, 1, t + 1)
      
      # update eWait and eQuit
      junk = rep(0, nTimeStep + 1)
      junk[t] = 1
      eWait =  gamma^stepGap * lambda * eWait + junk * c(action == "wait")
      eQuit = gamma ^stepGap * lambda * eQuit + c(action == "quit") 
      
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
