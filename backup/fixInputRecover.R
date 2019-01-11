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
source('subFxs/fittingFxs.R')

# need know the true parameter and true outputs
load('outputs/fixInputSimData/initialSpace.RData') # need nPara
################ initial start points space ############
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.005, 0.015, 0.035), each = 3 ^ 2)
startPoints[,2] = rep(c(1, 5, 15), each = 3 , 3)
startPoints[,3] = rep(c(0.75, 0.90, 0.95), 3^2)



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
  }else{
    load('outputs/fixInputSimData/rawLPData.RData')
    timeWaitedList = rawLPData$timeWaited[,1,]
    trialEarningsList = rawLPData$trialEarnings[,1,]
  }
  
  thisNegLLs = vector(length = nComb)
  thisSolutions = matrix(NA, nComb, nPara)
  
  nComb = 50
  res1 = microbenchmark({
    tempt = lapply(1 : nComb, function(combIdx, cond, wIni, trialEarningsList, timeWaitedList, startPoints)
      singleFitting(combIdx, cond, wIni, trialEarningsList[combIdx,], timeWaitedList[combIdx,], startPoints),
      cond = cond, wIni = wIni, trialEarningsList = trialEarningsList,
      timeWaitedList = timeWaitedList, startPoints = startPoints)
    
  }, times = 1L)

  
  res2 = microbenchmark({
    for(combIdx in 1 : nComb){
      timeWaited = timeWaitedList[combIdx,]
      trialEarnings = trialEarningsList[combIdx,]
      # initialize
      negLL = 1e10
      solution = vector(length = nPara)
      for(sIdx  in 1 : nrow(startPoints)){
        x0 = startPoints[sIdx, ]
        local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 10, stopval = 10)
        opts = list(algorithm = "NLOPT_LN_BOBYQA",maxeval = 10, stopval = 10,
                    local_optimizer = local_optimizer)
        # tau can't not be zero, otherwise the dominotor will be zero
        res = nloptr(x0 = x0, eval_f = negLLAction, lb = c(0, 1, 0) , ub = c(1, 22, 1),
                     opts = opts, cond = cond, wIni = wIni,
                     trialEarnings = trialEarnings, timeWaited =  timeWaited)
        if(res$objective < negLL){
          negLL = res$objective
          solution = res$solution
        }
        if(negLL < 10){
          break
        }
      }# end of all starting points
      thisNegLLs[combIdx] = negLL
      thisSolutions[combIdx, ] = solution
      if( (combIdx %% 25) == 0){
        txt = sprintf('complete %d percents', round(combIdx / nComb * 100))
        print(txt)      
      }
    }# end of all combId
  }, times = 1L)
  
  
  library('parallel')
  res3 = microbenchmark({
    tempt = mclapply(1 : nComb, function(combIdx)
      singleFitting(combIdx, cond, wIni, trialEarningsList[combIdx,], timeWaitedList[combIdx,], startPoints),
      mc.cores = 2)
  }, times = 1L)
  

  negLLs[[cond]] = thisNegLLs
  solutions[[cond]] = thisSolutions
}# end of all conditions
save(file = 'outputs/fixInputSimData/actionRecoverSmallPhi.RData', 'negLLs', 'solutions')

      
     
      
