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
  eWait = rep(0, nTimeStep); # es vector for "wait"
  eQuit = 0; # es vector for "quit"
  
  # recordings of vaWait and vaQuit
  vaWaits = matrix(NA, nTimeStep, blockSecs / iti + 1);
  vaWaits[,1] = wIni;
  vaQuits = matrix(NA, nTimeStep, blockSecs / iti + 1);
  vaQuits[1,1] = wIni* gamma ^(iti / stepDuration);
  
  # initialize time and reward seq
  totalSecs = 0
  seq = c()
  rewardDelays = rep(0, blockSecs / iti + 1)
  tIdx = 0
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0, blockSecs / iti + 1)
  timeWaited = rep(0, blockSecs / iti + 1)
  
  
  # initialize xs and action
  xs = 1 
  waitRate = 1 / sum(1  + exp((Qquit - Qwait[1])* tau))
  action = ifelse(runif(1) < waitRate, 'wait', 'quit')
  
  # loop until time runs out
      while(totalSecs < blockSecs) {
        tIdx = tIdx + 1
        # sample rewardDelay
        rewardDelay = drawSample(cond)
        
        # calculaye available time steps
        # since we use floor there maybe 0.5 sec error (less than 90 s)
        nAvaStep = min(floor((blockSecs - totalSecs) / stepDuration), nTimeStep)

        for(t in 1 : nAvaStep){
          # next reward 
          # determine whether reward occurs in the step t
          # the previous code is wrong, since rewards happens on 16s seconds woudldn't be counted 
          rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
          
          # if rewarded and wait, 5; otherwise, 0
          getReward = (action == 'wait' && rewardOccur);
          nextReward = ifelse(getReward, tokenValue, 0) 

          # dertime next state
          # go to the terminate state if at the final step or quit or reward arrives
          trialGoOn= (action == 'wait' && !rewardOccur && t < nAvaStep)
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
            timeWaited[tIdx] = ifelse(getReward,NA, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
            rewardDelays[tIdx] = rewardDelay
            break
          }
        }  # one trial end
        totalSecs = totalSecs + iti+ ifelse(getReward, rewardDelay, timeWaited[tIdx])
      } # simulation end
      outputs = list("Qwait" = Qwait,
                     "trialEarnings" = trialEarnings,
                     "timeWaited" = timeWaited,
                     "rewardDelays" = rewardDelays,
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