# this simulation using average payoff

### output file ####
outFile = 'outputs/QStarData'
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

################ input ################
# cond input
for(condIdx in 1 : 2){
  cond = conditions[condIdx];
  condColor = conditionColors[condIdx]
  sprintf('Condition : %s %s', cond, cond)
  
  # other input
  stepDuration = 0.5
  
  # genrate
  otherPara = getOtherPara(cond, stepDuration)
  ############# simulate for the distribution of toalEarnings ##########
  nPara = 3
  paraNames = c('phi', 'tau', 'gamma')
  nValue = 5
  nComb = nValue ^ nPara
  initialSpace = matrix(NA, nValue^nPara, nPara)
  initialSpace[,1] = rep(exp(seq(-4, -0.7, length.out = 5)), nValue^(nPara - 1)) # phi
  initialSpace[,2] = rep(rep(seq(2, 22, length.out = 5), each = nValue), nValue^(nPara - 2)) # tau
  initialSpace[,3] = rep(seq(0.8, 0.98, length.out = 5), each = nValue^2)
  save('initialSpace', 'nValue', 'nPara', 'paraNames', 'nComb',
       file = 'outputs/QStarData/initialSpace.RData')
  
  # set seed
  set.seed(123)
  
  # simualte 
  nRep = 5
  tMax = otherPara[['tMax']]
  TrialEarnings = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  RewardDelays = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  Qwait = array(dim = c(nValue^nPara, nRep, tMax / stepDuration)) # diifferent from master
  TimeWaited = array(dim = c(nValue^nPara, nRep, blockSecs / iti + 1))
  vaQuits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))
  vaWaits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, blockSecs / iti + 1))
  
  for(i in 1 : 1:nrow(initialSpace)){
    para = initialSpace[i,]
    for(j in 1 : nRep ){
      tempt=  simulationModel(para,otherPara, cond)
      TrialEarnings[i, j,] = tempt[['trialEarnings']]
      Qwait[i, j,] = tempt[['Qwait']]
      RewardDelays[i, j,] = tempt[['rewardDelays']]
      TimeWaited[i, j, ] = tempt[['timeWaited']]
      vaQuits[i, j,  , ] = tempt[['vaQuits']]
      vaWaits[i, j, ,  ] = tempt[['vaWaits']]
    }  
  }
  
  # organize and save outputs 
  outputData = list("Qwait" = Qwait, "timeWaited" = TimeWaited,
                    "rewardDelays" = RewardDelays, "trialEarnings" = TrialEarnings,
                    "vaWaits" = vaWaits, "vaQuits" = vaQuits
  )
  outFile = 'QStarData'
  if(cond == "HP"){
    rawHPData = outputData
    fileName = sprintf('outputs/%s/rawHPData.RData', outFile)
    save(rawHPData,file = fileName) 
  }else{
    rawLPData = outputData
    fileName = sprintf('outputs/%s/rawLPData.RData', outFile)
    save(rawLPData,file = fileName)
  }
}

######## generate hdrData ######
# hdrData include otherPara, MSPara
# also nTimeStep and TraceValue 
# therefore, no need to call getPara in later analysis anymore
stepDuration = 0.5
source("subFxs/paraFxs.R")
for(c in 1: 2){
  cond = conditions[c]
  otherPara = getOtherPara(cond, stepDuration)
  hdrData = otherPara
  hdrData$nTimeStep = hdrData$tMax / hdrData$stepDuration
  if(cond == 'HP') hdrHPData= hdrData else  hdrLPData= hdrData
}
fileName = 'outputs/QStarData/hdrData.RData'
save(hdrHPData, hdrLPData, file = fileName)
