# add number of repeation
simulationModel = function(para, otherPara, cond){
  # 
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = 1
  wIni = optimRewardRates[[cond]] / (1 - gamma)

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
  Qwait = rep(wIni, nTimeStep) # Q(si, ai = wait), any i
  Qquit = wIni
  es = rep(0, nTimeStep); # es vector for "wait"
  xs = 1 # every trial starts from the onset state
  
  # recordings of vaWait and vaQuit
  vaWaits = matrix(NA, nTimeStep, blockSecs / iti + 1);
  vaQuits = matrix(NA, nTimeStep, blockSecs / iti + 1);
  
  # initialize time and reward seq
  totalSecs = 0
  seq = c()
  rewardDelays = rep(0, blockSecs / iti + 1)
  tIdx = 0
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0, blockSecs / iti + 1)
  timeWaited = rep(0, blockSecs / iti + 1)
  
  # loop until time runs out
      while(totalSecs < blockSecs) {
        tIdx = tIdx + 1
        # sample rewardDelay
        rewardDelay = drawSample(cond)
        
        # calculaye available time steps
        # since we use floor there maybe 0.5 sec error (less than 90 s)
        nAvaStep = min(floor((blockSecs - totalSecs) / stepDuration), nTimeStep)
        
        # initialize action 
        waitRate = exp(Qwait[1] * tau) / sum(exp(Qwait[1]  * tau)  + exp(Qquit * tau))
        action = ifelse(runif(1) < waitRate, 'wait', 'quit')
        for(t in 1 : nAvaStep){
          # calculte action value 
          vaQuit = Qquit;
          vaWait = ws[xs];
          vaQuits[t, tIdx] = vaQuit;
          
          # determine action
          waitRate = exp(vaWait * tau) / sum(exp(vaWait * tau)  + exp(vaQuit * tau) )
          # when gamma is large, sometimes vaWait will be very large, sothat waitRate is NA
          if(is.na(waitRate)){
            waitRate = 1
          }
          action = ifelse(runif(1) < waitRate, 'wait', 'quit')
          
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
          
          # update eligilibity trace
          # here stepGap meatured between At and At-1
          junk = rep(0, nTimeStep)
          junk[xs] = 1
          es =  gamma^stepGap * lambda * es + junk * c(action == "wait")
          
          # update stepGap
          stepGap = ifelse(trialGoOn, 1, iti / stepDuration)
          
          # update action value of quit and wait
          # here stepGap meatured between At and At+1
          delta = nextReward + c(gamma^(stepGap) * max(ws[nextXs], vaQuit)) -
                                          ifelse(action == 'wait', vaWait, vaQuit)
          # anything wrong with the delta here?
          ws = ws + phi * delta * es
          
          # save changes in vaWaits, so vaWaited updated at t is used for t+1
          vaWaits[t, tIdx + 1] = ws[t];
          
          # update xs and stepGap
          xs = nextXs
          
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
      outputs = list("ws" = ws,
                     "trialEarnings" = trialEarnings,
                     "timeWaited" = timeWaited,
                     "rewardDelays" = rewardDelays,
                     "vaWaits" = vaWaits,
                     "vaQuits" = vaQuits)
      return(outputs)
} #end of the function

############ fitting model ###############
fittingModel = function(para, otherPara, inputData){
  # 
  phi = para[1]
  tau = para[2]
  gamma = para[3]
  lambda = para[4]
  wIni = para[5]
  
  
  # parse otherPara
  tMax= otherPara[['tMax']] 
  stepDuration = otherPara[['stepDuration']]
  timeTicks = otherPara[['timeTicks']] # begin timepoint of states
  nTimeStep = tMax / stepDuration
  
  # parse inputData
  nTrial = inputData[['nTrial']]
  rewardDelays = inputData[['rewardDelays']]
  waitDurations = inputData[['waitDurations']]
  trialEarnings = inputData[['trialEarnings']]
  
  # task para
  source("subFxs/wtwSettings.R")
  
  ########### simulation repeatedly ############
  # initialize action value, eligibility trace and current state xs
  ws = rep(wIni, nTimeStep) # weight vector for "wait", each element for each timeStep
  es = rep(0, nTimeStep); # es vector for "wait"
  xs = 1 # every trial starts from the onset state
  stepGap = 1 # since es = 0 initially, so this value is abitratry
  
  # initialize outputs 
  trialEarnings = rep(0, nTrial)
  timeWaited = rep(0, nTrial)
  vaWaits = matrix(NA, nTimeStep, nTrial);
  vaQuits = matrix(NA, tMax / stepDuration, nTrial);
  
  # loop over trials
  for(tIdx in 1 : nTrial){
    rewardDelay = rewardDelays[tIdx]
    # loop over time steps
    for(t in 1 : nTimeStep){
      # calculte action value 
      vaQuit = ws[1] * gamma^(iti / stepDuration) # didn't consider iTi, to stop getting things to complex
      vaWait = ws[xs];
      
      # determine action
      waitRate = exp(vaWait * tau) / sum(exp(vaWait * tau)  + exp(vaQuit * tau) )
      # when gamma is large, sometimes vaWait will be very large, sothat waitRate is NA
      if(is.na(waitRate)){
        waitRate = 1
      }
      action = ifelse(runif(1) < waitRate, 'wait', 'quit')
      
      # next reward 
      # determine whether reward occurs in the step t
      rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
      
      # if rewarded and wait, get the tokenValue; otherwise, 0
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
      
      # update eligilibity trace
      # here stepGap meatured between At and At-1
      junk = rep(0, nTimeStep)
      junk[xs] = 1
      es =  gamma^stepGap * lambda * es + junk * c(action == "wait")
      
      # update stepGap
      stepGap = ifelse(trialGoOn, 1, iti / stepDuration)
      
      # update action value of quit and wait
      # here stepGap meatured between At and At+1
      delta = nextReward + c(gamma^(stepGap) * max(ws[nextXs], vaQuit)) -
        ifelse(action == 'wait', vaWait, vaQuit)
      # anything wrong with the delta here?
      ws = ws + phi * delta * es
      
      # save history of vaWaits and vaQuits
      if(tIdx < nTrial) vaWaits[t, tIdx + 1] = ws[t];#  next trial vaWaits tracks updated vaWait, namely ws[t]

      vaQuits[t, tIdx] = vaQuit; # current trial vaQuits tracks current vaQuit
      
      # update xs 
      xs = nextXs
      
      # if break, update trialEarnings and timeWaited
      # return output
      if(!trialGoOn){
        trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
        # if quit, quit at t, if wait, wait until t+1
        timeWaited[tIdx] = ifelse(getReward,NA, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
        break
      }
    }  # one trial end
  } # simulation end
  outputs = list(
                 "trialEarnings" = trialEarnings,
                 "timeWaited" = timeWaited,
                 "vaWaits" = vaWaits,
                 "vaQuits" = vaQuits)
  return(outputs)
} #end of the function

