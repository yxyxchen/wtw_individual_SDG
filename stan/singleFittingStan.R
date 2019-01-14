# set up 
singleFittingStan = function(condIdx, combIdx){
  nChain = 2
  nCore = 5000
  
  # fitting the model
  source('subFxs/wtwSettings.R') 
  source('subFxs/fittingFxs.R')
  load('outputs/fixInputSimData/initialSpace.RData') # need nComb(for loop), wIni, nPara(for initialize solutions)
  
  # choose the condition
  cond = conditions[condIdx]
  wIni = wInis[[cond]]
  tMax = ifelse(cond == 'HP', tMaxs[1], tMaxs[2])
  nTimeStep = tMax / stepDuration
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
  
  
  # choose the combIdx
  timeWaited = timeWaitedList[combIdx,];
  trialEarnings = trialEarningsList[combIdx,];
  nTimePoints = round(ifelse(trialEarnings >0, ceiling(timeWaited / stepDuration), floor(timeWaited / stepDuration) + 1))
  data_list <- list(tMax = tMax,
                    wIni = wIni,
                    nTimeStep = nTimeStep,
                    N = length(timeWaited),
                    timeWaited = timeWaited,
                    trialEarnings = trialEarnings,
                    nTimePoints = nTimePoints)
  # init = list(list('phi' = 0.5, 'tau' = 15, 'gamma' = 0.5),
  #             list('phi' = 0.1, 'tau' = 5, 'gamma' = 0.1))
  tempt = sampling(object = model, data = data_list, cores = nCore, chains = nChain,
               iter = 5000, 
               show_messages = F) %>%
    rstan::extract(permuted = F, pars = c("phi", "tau", "gamma")) %>%
    adply(2, function(x) x) %>%  # change arrays into 2-d dataframe 
    select(-chains) %>% cbind(data.frame(combIdx = rep(combIdx, nCore * nChain), condition = rep(cond, nCore * nChain)))
  fileName = sprintf('stanOutPuts/fixInputSimData/%s_%d.txt', cond, combIdx)
  write.csv(tempt, file = fileName,row.names=FALSE)

}






