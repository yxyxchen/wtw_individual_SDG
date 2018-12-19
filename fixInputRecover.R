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
load('outputs/fixInputSimData/fixInputs.RData')# load rewardDelays

# need know the true parameter and true outputs
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/rawLPData.RData')
load('outputs/fixInputSimData/rawHPData.RData')
################ initial start points space ############
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.05, 0.15, 0.35), each = 3 ^ 2)
startPoints[,2] = rep(c(1, 5, 15), each = 3 , 3)
startPoints[,3] = rep(c(0.75, 0.90, 0.95), 3^2)
stepDuration = 0.5
################## recover parameters by MLE ##############
# loop across conditions
LLs = list()
solutions = list()
for(condIdx in 1 : 2){
  cond = conditions[condIdx]
  otherPara = getOtherPara(cond, stepDuration)
  thisRewardDelays = rewardDelays[[cond]]
  if(condIdx == 1) rawData = rawHPData else rawData = rawLPData
  wIni = wInis[[cond]]
  
  # initialize the output
  thisLLs = vector(length = nComb)
  thisSolutions = matrix(NA, nComb, nPara)
  
  for(combIdx in 1 : 62){
    
    para = initialSpace[combIdx, ]
    timeWaited = rawData$timeWaited[combIdx, 1, 1 : nTrials ]
    paste(sprintf('cond: %s, para: ', cond), round(para[1],2), para[2], para[3])
    # initialize
    LL = 1e5
    solution = vector(length = nPara)
    for(sIdx in 1 : nrow(startPoints)){
      x0 = startPoints[sIdx, ]
      local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e6)
      opts = list(algorithm = "NLOPT_LN_BOBYQA", stopval = 5,
                  local_optimizer = local_optimizer) 
      
      res = nloptr(x0 = x0, eval_f = eval_f_wait, lb = c(0, 0, 0) , ub = c(1, Inf, 1),
                   opts = opts,
                   otherPara = otherPara, cond = cond, wIni = wIni,
                   rewardDelays = thisRewardDelays, trueTimeWaited = timeWaited)
      if(res$objective < LL){
        LL = res$objective
        solution = res$solution
      }
    }# end of loop across starting points
    thisLLs[combIdx] = LL
    thisSolutions[combIdx,] = solution
  }
    LLs[[cond]] = thisLLs
    solutions[[cond]] = thisSolutions

}
save(file = 'outputs/fixInputSimData/waitRecover.RData', 'LLs', 'solutions')

#############



#################### for vaWaits and vaQuits ###############
# loop across conditions
LLs = list()
solutions = list()
for(condIdx in 1 : 2){
  cond = conditions[condIdx]
  otherPara = getOtherPara(cond, stepDuration)
  thisRewardDelays = rewardDelays[[cond]]
  if(condIdx == 1) rawData = rawHPData else rawData = rawLPData
  
  thisLLs = vector(length = nComb)
  thisSolutions = matrix(NA, nComb, nPara)
  
  for(combIdx in 1 : nComb){
    wIni = wInis[[cond]]
    para = initialSpace[combIdx, ]
    vaWaits =  rawData$vaWaits[combIdx, 1, , ]
    vaQuits = rawData$vaQuits[combIdx, 1, , ]
    trueDvs = transVaWaits(vaWaits) - transVaQuits(vaQuits)
    paste(sprintf('cond: %s, para: ', cond), round(para[1],2), para[2], para[3])
    # initialize
    LL = 1e5
    solution = vector(length = nPara)
    for(sIdx in 1 : nrow(startPoints)){
      x0 = startPoints[sIdx, ]
      local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e4)
      opts = list(algorithm = "NLOPT_LN_BOBYQA", stopval =10,
                  local_optimizer = local_optimizer) 
      res = nloptr(x0 = x0, eval_f = eval_f_dv, lb = c(0, 0, 0) , ub = c(1, Inf, 1),
                   opts = opts,
                   otherPara = otherPara, cond = cond, wIni = wIni,
                   rewardDelays = thisRewardDelays, trueDvs = trueDvs)
      if(res$objective < LL){
        LL = res$objective
        solution = res$solution
      }
    }# end of loop across starting points
    thisLLs[combIdx] = LL
    thisSolutions[combIdx,] = solution
  }
  LLs[[cond]] = thisLLs
  solutions[[cond]] = thisSolutions
  
}
save(file = 'outputs/fixInputSimData/dvRecover.RData', 'LLs', 'solutions')





x0 = c(0.1, 5, 0.5)


