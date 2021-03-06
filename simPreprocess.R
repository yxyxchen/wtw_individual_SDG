# this script preprocesses the simulation data 
# first, it calculates wtw timeseries and AUC for the simulation data 
# second, it callopses the simulation data for the later group analysis


############# load data and functions ######
load('outputs/simData/rawHPData.RData')
load('outputs/simData/rawLPData.RData')
library("dplyr")
library("tidyr")
source('subFxs/plotThemes.R')
source('subFxs/wtwSettings.R')
source('subFxs/helperFxs.R')


############ calculate colpData ########
# colpTrialEarnings 
colpTrialEarnings = vector(mode = "list", 2)
colpTotalEarnings = vector(mode = "list", 2)
for(c in 1 : 2){
  cond = conditions[c];
  
  if(cond == "HP") inputData = rawHPData else inputData = rawLPData
  
  colpTrialEarnings[[cond]] =
    apply(inputData$trialEarnings, MARGIN = c(1,3), FUN = mean)
  colpTotalEarnings[[cond]] = apply(colpTrialEarnings[[cond]],
                                        MARGIN = 1, FUN = sum)
}

# colpAUC
colpAUC = list()
rawWTW = list()
for(c in 1 : 2){
  cond = conditions[c];
  
  # input
  if(cond== "HP") inputData = rawHPData else inputData = rawLPData
  tMax = tMaxs[c]
  trialTick = trialTicks[[cond]]
  
  # dim 
  nComb = dim(inputData$timeWaited)[1]
  nRep = dim(inputData$timeWaited)[2]
  
  # temporary output
  output = matrix(NA, nComb, nRep)
  wtwResult = array(dim = c(nComb, nRep, length(tGrid)))
  for(i in 1 : nComb){
    for(j in 1 : nRep){
      waitDuration = inputData$timeWaited[i, j, ]
      rewardDelay = inputData$rewardDelays[i, j, ]
      quitIdx = (inputData$trialEarnings[i, j, ] == 0)
      
      waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
      endTick = match(0,rewardDelay)
      waitDuration = waitDuration[1 : (endTick - 1)]
      quitIdx = quitIdx[1 : (endTick - 1)]
      
      output[i, j] = kmscSimple(waitDuration, quitIdx, tMax, trialTick)$auc
      wtwResult[i, j, ] = wtwTSSimple(waitDuration, quitIdx, tGrid, tMax)
    } # end of comb
  }# end of condition
  colpAUC[[cond]] = rowSums(output) / ncol(output)
  rawWTW[[cond]] = wtwResult
}


########## save data #########
colpHPData = list(totalEarnings = colpTotalEarnings$HP,
                  trialEarnings = colpTrialEarnings$HP,
                  AUC = colpAUC$HP,
                  wtw = apply(rawWTW$HP, MARGIN = 1, mean))
colpLPData = list(totalEarnings = colpTotalEarnings$LP,
                  trialEarnings = colpTrialEarnings$LP,
                  AUC = colpAUC$LP,
                  wtw = apply(rawWTW$LP, MARGIN = 1, mean)
)
fileName = "outputs/simData/colpData.RData"
save('colpLPData', 'colpHPData', file = fileName )
fileName = "outputs/simData/rawWTW.RData"
save('rawWTW', file = fileName)
