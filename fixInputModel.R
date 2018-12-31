# add number of repeation
fixInputModel = function(para, otherPara, cond, wIni, rewardDelays, ratio, steep){
  # set.seed(123)
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = 1
  # wIni = optimRewardRates[[cond]] / (1 - gamma)
  
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
  Qwait = wIni * exp(-steep * (0 : (nTimeStep - 1))) # Q(si, ai = wait), any i
  Qquit =  Qwait[1] * ratio
  
  # recordings of vaWait and vaQuit
  vaWaits = matrix(NA, nTimeStep, nTrial);
  vaWaits[,1] = Qwait;
  vaQuits = vector(length = nTrial);
  vaQuits[1] = Qquit;
  
  # initialize outputs 
  trialEarnings = rep(0, nTrial)
  timeWaited = rep(0, nTrial)
  
  # loop until time runs out
  tIdx = 1
  while(tIdx <= nTrial) {
    # sample rewardDelay
    rewardDelay = rewardDelays[tIdx]
    # calculaye available time steps
    # since we use floor there maybe 0.5 sec error (less than 90 s)
    
    # loop until available steps run out 
    t = 1
    while(t <= nTimeStep){
      # action
      waitRate =  1 / sum(1  + exp((Qquit - Qwait[t])* tau))
      action = ifelse(runif(1) < waitRate, 'wait', 'quit')
      # next reward 
      rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
      getReward = (action == 'wait' && rewardOccur);
      nextReward = ifelse(getReward, tokenValue, 0) 
      
      # dertime next state
      # go to the terminate state if at the final step or quit or reward arrives
      trialGoOn= (action == 'wait' && !rewardOccur && t < nTimeStep)
      
      # if the trial stops, track trialEarnings, timeWaited and rewardDelays
      # otherwise, continue
      if(!trialGoOn){
        trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
        # since it make no sense to wait after the reward arrives, 
        # when getting rewards, quitting immediately 
        timeWaited[tIdx] = ifelse(getReward, rewardDelay, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
        break
      }else{
        t = t + 1
      }
      
    }
    
    # update Qwait and Qquit if it is not the last trial
    if(tIdx < nTrial){
      if(nextReward == 0){
        nextWaitRateHat =  1 / sum(1  + exp((Qquit - Qwait[1])* tau))
        trialReward = nextWaitRateHat * Qwait[1] * gamma ^(iti / stepDuration) +
          (1 - nextWaitRateHat) * Qquit * gamma ^(iti / stepDuration)
      }else{
        trialReward = nextReward
      }
      
      if(action == 'wait'){
        Qwait[1 : t] = (1 - phi) * Qwait[1 : t] + phi * trialReward * gamma ^ rev((0 : (t - 1 )))
      }else{
        Qquit =  (1 - phi) * Qquit + phi *  trialReward
        if(t > 1){
          Qwait[1 : (t - 1)] = (1 - phi) * Qwait[1 : (t - 1)] +
            phi * trialReward * gamma ^ rev((1 : (t - 1 )))
        }
      }
      # track vaWaits and vaQuits 
      vaWaits[,tIdx + 1] = Qwait
      vaQuits[tIdx + 1] = Qquit
    }
    
    # go to the next trial 
    tIdx = tIdx + 1
    
    
  } # simulation end
  outputs = list(
                 "trialEarnings" = trialEarnings,
                 "timeWaited" = timeWaited,
                 "vaWaits" = vaWaits,
                 "vaQuits" = vaQuits)
  return(outputs)
} #end of the function

########################------------------------########################
# seqModel,which receives rewardDelays sequences #
########################------------------------########################

seqModel = function(para, otherPara, cond, wIni, rewardDelays){
  # set.seed(123)
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
  outputs = list(
    "trialEarnings" = trialEarnings,
    "timeWaited" = timeWaited)
  return(outputs)
} #end of the function
