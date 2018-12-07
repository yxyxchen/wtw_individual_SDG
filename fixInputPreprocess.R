# this script collapses fix-input simualtion data on the para level

# load scripts, library and
source('subFxs/wtwSettings.R') 
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/rawHPData.RData')
load('outputs/fixInputSimData/rawLPData.RData')
load('outputs/fixInputSimData/fixInputs.RData')
library('dplyr')
library('tidyr')
source('subFxs/helperFxs.R')


################ compare waitDurations ##############
waitDelta = list()
dvDelta = list()
for(cIdx in 1 : 2){
  cond = conditions[cIdx]
  if(cond == 'HP') inputData = rawHPData
  else  inputData = rawLPData
  
  # initialize
  thisWaitDelta = matrix(NA, nComb, nComb)
  thisDvDelta = matrix(NA, nComb, nComb)
  # across
  for(i in 1 : (nComb - 1)){
    for(j in (i + 1) : nComb){
      thisWaitDelta[i, j] = mean(abs(inputData$timeWaited[i,1, ] - inputData$timeWaited[j,1,]))
      thisWaitDelta[j, i] =  thisWaitDelta[i, j]
      thisDvDelta[i, j] = mean(abs(inputData$dvs[i,1, , ] - inputData$dvs[j,1, , ]))
      thisDvDelta[j, i]  = thisDvDelta[i, j]
    }
  }
  # within
  for(i in 1 : nComb){
    thisWaitDelta[i, i] =  mean(abs(inputData$timeWaited[i,1, ] - inputData$timeWaited[i,2,]))
    thisDvDelta[i, i] =  mean(abs(inputData$dvs[i,1, , ] - inputData$dvs[i,2, , ]))
  }
  # save
  waitDelta[[cond]] = thisWaitDelta
  dvDelta[[cond]] = thisDvDelta
}

############# withinPara #####
junk = vector(length = nComb)
waitWithinDelta = list(HP = junk, LP = junk)
dvWithinDelta = list(HP = junk, LP = junk)
for(i in 1 : nComb){
  waitWithinDelta$HP[i] = waitDelta$HP[i,i]
  waitWithinDelta$LP[i] = waitDelta$LP[i,i]    
  dvWithinDelta$HP[i] = dvDelta$HP[i,i] 
  dvWithinDelta$LP[i] = dvDelta$LP[i,i] 
}


junk = vector(length = nComb)
waitAcrossDelta = list(HP = junk, LP = junk)
dvAcrossDelta = list(HP = junk, LP = junk)
for(i in 1 : nComb){
  waitAcrossDelta$HP[i] = sum(waitDelta$HP[i,], na.rm = T) / (nComb - 1)
  waitAcrossDelta$LP[i] = sum(waitDelta$LP[i,], na.rm = T) / (nComb - 1) 
  dvAcrossDelta$HP[i] = sum(dvDelta$HP[i,], na.rm = T) / (nComb - 1)
  dvAcrossDelta$LP[i] = sum(dvDelta$LP[i,], na.rm = T) / (nComb - 1)
}

junk = vector(length = nComb)
waitSucessRecover = list(HP = junk, LP = junk)
dvSucessRecover = list(HP = junk, LP = junk)
for(i in 1 : nComb){
  waitSucessRecover$HP[i] = sum(waitDelta$HP[i,i] < waitDelta$HP[i, 1 : nComb != i]) / (nComb - 1)
  waitSucessRecover$LP[i]=  sum(waitDelta$LP[i,i] < waitDelta$LP[i, 1 : nComb != i]) / (nComb - 1)  
  dvSucessRecover$HP[i] = sum(dvDelta$HP[i,i] < dvDelta$HP[i, 1 : nComb != i]) / (nComb - 1)
  dvSucessRecover$LP[i]=  sum(dvDelta$LP[i,i] < dvDelta$LP[i, 1 : nComb != i]) / (nComb - 1)    
}

############# AUC ############
AUC = list()
for(c in 1 : 2){
  cond = conditions[c];
  
  # input
  if(cond== "HP") inputData = rawHPData else inputData = rawLPData
  tMax = tMaxs[c]
  trialTick = trialTicks[[cond]]
  
  # temporary output
  output = matrix(NA, nComb, nRep)
  for(i in 1 : nComb){
    for(j in 1 : nRep){
      waitDuration = inputData$timeWaited[i, j, ]
      rewardDelay = rewardDelays[[cond]]
      quitIdx = (inputData$trialEarnings[i, j, ] == 0)
      
      output[i, j] = kmscSimple(waitDuration, quitIdx, tMax, trialTick)$auc
    } # end of comb
  }# end of condition
  AUC[[cond]] = rowSums(output) / ncol(output)
}

########## save data #########
colpHPData = data.frame(waitSucessRecover  = waitSucessRecover$HP,
                  waitWithinDelta = waitWithinDelta$HP,
                  waitAcrossDelta = waitAcrossDelta$HP,
                  dvSucessRecover = dvSucessRecover$HP,
                  dvWithinDelta = dvWithinDelta$HP,
                  dvAcrossDelta = dvAcrossDelta$HP,
                  AUC = AUC$HP)
colpLPData = data.frame(waitSucessRecover  = waitSucessRecover$LP,
                  waitWithinDelta = waitWithinDelta$LP,
                  waitAcrossDelta = waitAcrossDelta$LP,
                  dvSucessRecover = dvSucessRecover$LP,
                  dvWithinDelta = dvWithinDelta$LP,
                  dvAcrossDelta = dvAcrossDelta$LP,
                  AUC = AUC$LP)
save('colpLPData', 'colpHPData', file = 'outputs/fixInputSimData/colpData.RData' )

