# add number of repeation
simulationModel = function(para, otherPara, cond, wIni){
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
  
  ########### simulation repeatedly ############
  # initialize action value, eligibility trace and stat
  # exp(-r * stepDuration) = gamma
  Qwait = rep(wIni, nTimeStep) # Q(si, ai = wait), any i
  Qquit = wIni * gamma ^(iti / stepDuration)
  
  # recordings of vaWait and vaQuit
  vaWaits = matrix(NA, nTimeStep, blockSecs / iti + 1);
  vaWaits[,1] = Qwait;
  vaQuits = vector(length = blockSecs / iti + 1);
  vaQuits[1] = Qquit;
  
  # initialize time and reward seq
  totalSecs = 0
  rewardDelays = rep(0, blockSecs / iti + 1)
  
  # initialize outputs 
  trialEarnings = rep(0, blockSecs / iti + 1)
  timeWaited = rep(0, blockSecs / iti + 1)
  
  # loop until time runs out
  tIdx = 1
  while(totalSecs < blockSecs) {
    # sample rewardDelay
    rewardDelay = drawSample(cond)
    # calculaye available time steps
    # since we use floor there maybe 0.5 sec error (less than 90 s)
    nAvaStep = min(floor((blockSecs - totalSecs) / stepDuration), nTimeStep)
    
    # loop until available steps run out 
    t = 1
    while(t <= nAvaStep){
      # action
      waitRate =  1 / sum(1  + exp((Qquit - Qwait[t])* tau))
      action = ifelse(runif(1) < waitRate, 'wait', 'quit')
      # next reward 
      rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
      getReward = (action == 'wait' && rewardOccur);
      nextReward = ifelse(getReward, tokenValue, 0) 
    
      # dertime next state
      # go to the terminate state if at the final step or quit or reward arrives
      trialGoOn= (action == 'wait' && !rewardOccur && t < nAvaStep)
      
      # if the trial stops, track trialEarnings, timeWaited and rewardDelays
      # otherwise, continue
      if(!trialGoOn){
        trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
        # since it make no sense to wait after the reward arrives, 
        # when getting rewards, quitting immediately 
        timeWaited[tIdx] = ifelse(getReward, rewardDelay, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
        rewardDelays[tIdx] = rewardDelay
        break
      }else{
        t = t + 1
      }
      
    }
    
    # update totalSecs
    totalSecs = totalSecs + iti+ ifelse(getReward, rewardDelay, timeWaited[tIdx]) 
    
    # update Qwait and Qquit if it is not the last trial
    if(totalSecs < blockSecs){
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
    
    if(Qquit < 0 || sum(Qwait < 0) > 0){
      browser()
    }
  } # simulation end
  outputs = list(
                 "trialEarnings" = trialEarnings,
                 "timeWaited" = timeWaited,
                 "rewardDelays" = rewardDelays,
                 "vaWaits" = vaWaits,
                 "vaQuits" = vaQuits)
  return(outputs)
} #end of the function

