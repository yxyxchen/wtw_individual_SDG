# 
singleFitting = function(combIdx, cond, wIni, trialEarnings, timeWaited, startPoints){
  negLL = 1e10
  solution = vector(length = nPara)
  for(sIdx  in 1 : nrow(startPoints)){
    x0 = startPoints[sIdx, ]
    local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e1, stopval = 10)
    opts = list(algorithm = "NLOPT_LN_BOBYQA",maxeval = 1e1, stopval = 10,
                local_optimizer = local_optimizer)
    # tau can't not be zero, otherwise the dominotor will be zero
    res = nloptr(x0 = x0, eval_f = negLLAction, lb = c(0, 1, 0) , ub = c(1, 30, 1),
                 opts = opts, cond = cond, wIni = wIni,
                 trialEarnings = trialEarnings, timeWaited =  timeWaited)
    if(res$objective < negLL){
      negLL = res$objective
      solution = res$solution
    }
    if(negLL < 10){
      outputs = c(negLL, solution)
      return(outputs)
      break
    }
  }# end of all starting points
  if( (combIdx %% 2) == 0){
    txt = sprintf('complete %d percents', round(combIdx / nComb * 100))
    print(txt)      
  }
  outputs = c(negLL, solution)
  return(outputs)
}


# this function defines neg loglikelihood for fitting
negLLAction  = function(x, cond, wIni, trialEarnings, timeWaited){
  # parse x
  phi = x[1]
  tau = x[2]
  gamma = x[3]
  
  # parse otherPara
  tMax= ifelse(cond == 'HP', tMaxs[1], tMaxs[2])
  nTrial = length(trialEarnings)
  nTimeStep = tMax / stepDuration
  
  # initialize action values, eligibility traces and lik
  Qwait = rep(wIni, nTimeStep) 
  Qquit = wIni * gamma ^(iti / stepDuration)
  LL = 0
  
  # need to skip 
  for(tIdx in 1 : nTrial){
    # determine nTimePoint
    thisTimeWaited = timeWaited[tIdx]
    nTimePoint = ifelse(trialEarnings[tIdx] > 0,
                        ceiling(thisTimeWaited / stepDuration),
                        floor(thisTimeWaited / stepDuration) + 1
    )
    if(is.na(LL)) browser()
    # try mcApply
    if(trialEarnings[tIdx] == 0){
      select = 1 : (nTimePoint - 1)
      LL = LL + sum(mapply(function(x, y) tau * x - log(exp(tau * x) + exp(tau * y)),
                           Qwait[select], rep(Qquit, length(select))))
      LL = LL + tau * Qquit - log(exp(tau * Qwait[nTimePoint]) + exp(tau * Qquit))
    }else{
      select = 1 : nTimePoint
      LL = LL + sum(mapply(function(x, y) tau * x - log(exp(tau * x) + exp(tau * y)),
                           Qwait[select], rep(Qquit, length(select))))
    }
    
    # update Qwait and Qquit 
    if(trialEarnings[tIdx] > 0){
      trialReward = tokenValue
      Qwait[1 : nTimePoint] = (1 - phi) * Qwait[1 : nTimePoint] + phi * trialReward * gamma ^ rev((0 : (nTimePoint - 1 )))
    }else{
      nextWaitRateHat =  1 / sum(1  + exp((Qquit - Qwait[1])* tau))
      trialReward = nextWaitRateHat * Qwait[1] * gamma ^(iti / stepDuration) +
        (1 - nextWaitRateHat) * Qquit * gamma ^(iti / stepDuration)
      
      Qquit =  (1 - phi) * Qquit + phi *  trialReward
      if(nTimePoint > 1){
        Qwait[1 : (nTimePoint - 1)] = (1 - phi) * Qwait[1 : (nTimePoint - 1)] +
          phi * trialReward * gamma ^ rev((1 : (nTimePoint - 1 )))
      }
    }# end of one trial
  }# end of all trials
  negLL = -LL
  return(negLL)
} #end of the function