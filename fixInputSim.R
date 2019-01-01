# load stepDuration, scripts and libraries
stepDuration = 0.5
source('subFxs/wtwSettings.R') # wtw settings for both HP and LP
source('subFxs/paraFxs.R') 
source('subFxs/taskFxs.R') 
library('ggplot2')
library('dplyr')
library('tidyr')
source('fixInputModel.R')

# output files
dir.create('outputs/fixInputSimData')
dir.create('outputs/fixInputSim_figures')
############# calculate and save simualtion parameters #########
# run for N trials
nTrials = 50
nRep = 5

# determine reward delays
rewardDelays = list()
for(cIdx in 1 : 2){
  # set.seed to make sure the reward delays and the simulations are reproduceble 
  set.seed(123)
  cond = conditions[cIdx] 
  tempt = vector(length = nTrials)
  for(i in 1 : nTrials){
    tempt[i] = drawSample(cond)
  }
  rewardDelays[[cond]] = tempt
}
save('nTrials', 'nRep', 'rewardDelays', file = 'outputs/fixInputSimData/fixInputs.RData')

################## simulation ##########
set.seed(123)
for(cIdx in 1 : 2){
  # define the condition
  cond = conditions[cIdx]
  # get otherPara given the condition
  otherPara = getOtherPara(cond, stepDuration)
  tMax = otherPara$tMax
  wIni = wInis[[cond]]
  
  # 
  thisRewardDelays = rewardDelays[[cond]]
 
  # initialize outputs
  TrialEarnings = array(dim = c(nValue^nPara, nRep, nTrials))
  TimeWaited = array(dim = c(nValue^nPara, nRep, nTrials))
  vaQuits = array(dim = c(nValue^nPara, nRep, nTrials))
  vaWaits = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, nTrials))
  dvs = array(dim = c(nValue^nPara, nRep, tMax / stepDuration, nTrials)) # decision value, Qwait - Qquit, scaled within trials
  # simulate for every para
  for(i in 1 : nComb){
    para = initialSpace[i, ]
    for(j in 1 : nRep){
      tempt = fixInputModel(para, otherPara, cond, wIni, thisRewardDelays)
      junk = tempt[['trialEarnings']]
      TrialEarnings[i, j, ] = junk[1 : nTrials]
      junk =  tempt[['timeWaited']]
      TimeWaited[i, j, ] = junk[1 : nTrials]
      vaQuits[i, j,  ] = tempt[['vaQuits']]
      vaWaits[i, j, ,  ]  = tempt[['vaWaits']]
      # decision value
      junk = vaWaits[i, j, ,  ] - vaQuits[i, j, ]
      dvs[i, j, , ] = junk
    }
  }

  # organize and save outputs 
  outputData = list("timeWaited" = TimeWaited, "trialEarnings" = TrialEarnings,
                    "vaWaits" = vaWaits, "vaQuits" = vaQuits, 'dvs' = dvs
  )
  
  if(cond == "HP"){
    rawHPData = outputData
    save(rawHPData,file = 'outputs/fixInputSimData/rawHPData.RData') 
  }else{
    rawLPData = outputData
    save(rawLPData,file = 'outputs/fixInputSimData/rawLPData.RData') 
  }
  
}

