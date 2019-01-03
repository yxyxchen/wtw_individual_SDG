########### load scripts, library ################
# basic
library('dplyr')
library('tidyr')
library('ggplot2')
source('subFxs/plotThemes.R')
library(nloptr)
# resimulate the
source('subFxs/wtwSettings.R') 
source('subFxs/paraFxs.R') 
source('subFxs/taskFxs.R')
source('eval_f.R')
library('scales')
load('outputs/fixInputSimData/fixInputs.RData')# load rewardDelays

# need know the true parameter and true outputs
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/rawLPData.RData')
load('outputs/fixInputSimData/rawHPData.RData')
################ initial start points space ############
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.005, 0.015, 0.035), each = 3 ^ 2)
startPoints[,2] = rep(c(1, 5, 15), each = 3 , 3)
startPoints[,3] = rep(c(0.75, 0.90, 0.95), 3^2)
stepDuration = 0.5


#################### for vaWaits and vaQuits ###############
# loop across conditions

negLLs = list()
solutions = list()
for(condIdx in 1 : 2){
  cond = conditions[condIdx]
  otherPara = getOtherPara(cond, stepDuration)
  thisRewardDelays = rewardDelays[[cond]]
  if(condIdx == 1) rawData = rawHPData else rawData = rawLPData
  
  thisNegLLs = vector(length = nComb)
  thisSolutions = matrix(NA, nComb, nPara)
  
  for(combIdx in 1 : nComb){
    wIni = wInis[[cond]]
    para = initialSpace[combIdx, ]
    timeWaited = rawData$timeWaited[combIdx, 1, 1 : nTrials ]
    trialEarnings = rawData$trialEarnings[combIdx, 1, 1 : nTrials ]
    paste(sprintf('cond: %s, para: ', cond), round(para[1],2), para[2], para[3])
    # initialize
    negLL = 1e10
    solution = vector(length = nPara)
    for(sIdx  in 1 : nrow(startPoints)){
      x0 = startPoints[sIdx, ]
      local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e3, stopval = 5)
      opts = list(algorithm = "NLOPT_LN_BOBYQA",maxeval = 1e3, stopval = 5,
                  local_optimizer = local_optimizer)
      # tau can't not be zero, otherwise the dominotor will be zero
      res = nloptr(x0 = x0, eval_f = negLLAction, lb = c(0, 1, 0) , ub = c(1, 22, 1),
                   opts = opts,
                   otherPara = otherPara, cond = cond, wIni = wIni,
                   trialEarnings = trialEarnings, timeWaited =  timeWaited)
      if(res$objective < negLL){
        negLL = res$objective
        solution = res$solution
      }
      if(negLL < 25){
        break
      }
    }# end of all starting points
    if( (combIdx %% 25) == 0){
      txt = sprintf('complete %s', percent(combIdx / nComb))
      print(txt)      
    }
  }# end of all combId
  negLLs[[cond]] = thisNegLLs
  solutions[[cond]] = thisSolutions
}# end of all conditions
save(file = 'outputs/fixInputSimData/actionRecoverSmallPhi.RData', 'negLLs', 'solutions')

      
     
      
