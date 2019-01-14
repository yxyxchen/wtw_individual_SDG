# this simulation using average payoff

### output file ####
outFile = 'outputs/simData'
dir.create(outFile)
################## read data #################
# library 
library('ggplot2')
library('dplyr')
library('tidyr')
source('model.R') # QStar model
source('subFxs/wtwSettings.R') # wtw settings for both HP and LP
                        # can't change
source('subFxs/paraFxs.R') # functions to get MSPara and otherPara from inputs and wtwSettings
                    # can change for different MS model, and 
stepDuration = 0.5
############# generate the parameter search space ##########
nPara = 3
paraNames = c('phi', 'tau', 'gamma')
nValue = 5
nComb = nValue ^ nPara
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.1, 0.5, by = 0.1), nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(2, 22, length.out = 5), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(seq(0.8, 0.98, length.out = 5), each = nValue^2)


### calculate wIni
wInis = list()
for(c in 1 : 2){
  cond = conditions[c];
  trialTick = trialTicks[[cond]]
  thisDelayPDF = rewardDelayPDF[[cond]]
  nTicks = length(trialTick)
  
  # assume gamma = 0.9
  gamma = 0.9
  r = - log(gamma) / stepDuration
  actionValueWaits = rep(0, nTicks)
  for(k in 1 : nTicks){
    actionValueWaits[k] = sum(tokenValue * exp(- (trialTick[k : nTicks] - trialTick[k]) * r)* thisDelayPDF[k : nTicks] / sum( thisDelayPDF[k : nTicks]))    
  }
  junk = mean(actionValueWaits)    
  wInis[[cond]] = junk
}

nRep = 5
count = t(matrix(1 : (nComb * nRep), nRep, nComb))
save('initialSpace', 'nValue', 'nPara', 'paraNames', 'nComb', 'wInis', 'count',
     file = 'outputs/simData/initialSpace.RData')


################ input ################
# cond input
for(condIdx in 1 : 2){
  thisPackData = vector(length = nComb * nRep, mode ='list')
  cond = conditions[condIdx];
  wIni = wInis[[cond]]
  
  sprintf('Condition : %s %s', cond, cond)
  
  # other input
  stepDuration = 0.5
  
  # genrate
  otherPara = getOtherPara(cond, stepDuration)
  nTimeStep = otherPara$tMax / stepDuration
  
  # set seed
  set.seed(123)
  
  # simualte 
  tMax = otherPara[['tMax']]
  TrialEarnings = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  RewardDelays = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  Qwait = array(dim = c(nValue^nPara, nRep, tMax / stepDuration)) # diifferent from master
  TimeWaited = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  vaQuits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))
  vaWaits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))
  
  for(h in 1 : nrow(initialSpace)){
    para = initialSpace[h,];
    # calculate wIni
    for(j in 1 : nRep ){
      tempt=  simulationModel(para,otherPara, cond, wIni)
      TrialEarnings[h, j,] = tempt[['trialEarnings']]
      Qwait[h, j,] = tempt[['Qwait']]
      RewardDelays[h, j,] = tempt[['rewardDelays']]
      TimeWaited[h, j, ] = tempt[['timeWaited']]
      vaQuits[h, j,  , ] = tempt[['vaQuits']]
      vaWaits[h, j, ,  ] = tempt[['vaWaits']]
      thisPackData[[count[h, j]]] = tempt
    }  
  }
  
  # organize and save outputs 
  outputData = list("Qwait" = Qwait, "timeWaited" = TimeWaited,
                    "rewardDelays" = RewardDelays, "trialEarnings" = TrialEarnings,
                    "vaWaits" = vaWaits, "vaQuits" = vaQuits
  )
  
    
  outFile = 'simData'
  if(cond == "HP"){
    rawHPData = outputData
    packHPData = thisPackData
    fileName = sprintf('outputs/%s/rawHPData.RData', outFile)
    save(rawHPData,file = fileName) 
    fileName = sprintf('outputs/%s/packHPData.RData', outFile)
    save(packHPData,file = fileName)
  }else{
    rawLPData = outputData
    packLPData = thisPackData
    fileName = sprintf('outputs/%s/rawLPData.RData', outFile)
    save(rawLPData,file = fileName)
    fileName = sprintf('outputs/%s/packLPData.RData', outFile)
    save(packLPData,file = fileName)
  }
}

