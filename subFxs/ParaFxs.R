
# getOtherPara organize steoDurations together with timing-variables in wtwSetting
getOtherPara = function(cond, stepDuration, holdOnsteps = 5){
  tMax = ifelse(cond == conditions[1], tMaxs[1], tMaxs[2])
  
  # check validity of step duration
  if(1 %% stepDuration != 0){
    return("stepDuration is not divisible")
  }
  
  # get nTimeStep and timeTicks
  nTimeStep = tMax / (stepDuration);
  timeTicks = seq(0, tMax, length.out = nTimeStep + 1);

  # pass to otherPara
  otherPara = list()
  otherPara[['tMax']] = tMax
  otherPara[['timeTicks']] = timeTicks
  otherPara[['stepDuration']] = stepDuration
  otherPara[['holdOnSteps']] = holdOnsteps # not used in the new model: simulate.R
  
  # return
  return(otherPara)
}

getMSPara = function(cond, stepDuration, nMS, traceDecay, sigma){
  tMax = ifelse(cond == 'unif16', 16, 32)
  nTimeStep = tMax / (stepDuration); 
  traceValues = traceDecay ^ ((1 : nTimeStep) - 1)
  
  # mu for MS
  nMS = 10; # number of microstimuli
  junk = seq(1, traceValues[length(traceValues)], length.out = nMS)# mean of the basis function for each MS
  MSMus = vector(length = nMS);
  MSTimeSteps = vector(length = nMS);
  for(i in 1 : length(junk)){
    MSTimeSteps[i] = order(abs(traceValues - junk[i]))[1]
    MSMus[i] = traceValues[MSTimeSteps[i] ]
  }
  
  # pass to MSPara
  MSPara = list()
  MSPara[['traceDecay']]  = traceDecay
  MSPara[['MSMus']] = MSMus
  MSPara[['sigma']]  = sigma
  MSPara[["nMS"]] = nMS
  
  # return
  return(MSPara)
}


