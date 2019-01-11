########### load scripts, library ################
# basic

library('parallel')
library(nloptr)
# resimulate the
source('subFxs/wtwSettings.R') 
source('subFxs/fittingFxs.R')

# need know the true parameter and true outputs
load('outputs/fixInputSimData/initialSpace.RData') # need nComb(for loop), wIni, nPara(for initialize)
################ initial start points space ############
# non-informative starting points 
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.1, 0.5, 0.9), each = 3 ^ 2)
startPoints[,2] = rep(c(2, 15, 28), each = 3 , 3)
startPoints[,3] = rep(c(0.1, 0.5, 0.90), 3^2)

# informative starting points 
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.005, 0.035, 0.06), each = 3 ^ 2)
startPoints[,2] = rep(c(2, 15, 28), each = 3 , 3)
startPoints[,3] = rep(c(0.75, 0.87, 0.99), 3^2)
#################### for vaWaits and vaQuits ###############
# loop across conditions
negLLs = list()
solutions = list()
for(condIdx in 1 : 2){
  cond = conditions[condIdx]
  wIni = wInis[[cond]]
  if(condIdx == 1){
    load('outputs/fixInputSimData/rawHPData.RData')
    timeWaitedList = rawHPData$timeWaited[,1,]
    trialEarningsList = rawHPData$trialEarnings[,1,]
    rm(rawHPData)
  }else{
    load('outputs/fixInputSimData/rawLPData.RData')
    timeWaitedList = rawLPData$timeWaited[,1,]
    trialEarningsList = rawLPData$trialEarnings[,1,]
    rm(rawLPData)
  }
  
  thisNegLLs = vector(length = nComb)
  thisSolutions = matrix(NA, nComb, nPara)
  
  tempt = mclapply(1 : nComb, function(combIdx)
    singleFitting(combIdx, cond, wIni, trialEarningsList[combIdx,], timeWaitedList[combIdx,], startPoints),
    mc.cores = 2)
  
  junk = matrix(unlist(tempt), nrow = nComb, byrow = T)
  thisNegLLs = junk[,1]
  thisSolutions = junk[,2:4]
  negLLs[[cond]] = thisNegLLs
  solutions[[cond]] = thisSolutions
}# end of all conditions
save(file = 'outputs/fixInputSimData/actionRecoverSmallPhiNI.RData', 'negLLs', 'solutions')

      
     
      
