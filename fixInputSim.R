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


############# generate the parameter search space ##########
# use different parameters for simulation and fixInput simulation
# here we need to make sure the parameters are similiar to the true parameters
nPara = 3
paraNames = c('phi', 'tau', 'gamma')
nValue = 5
nComb = nValue ^ nPara
initialSpace = matrix(NA, nValue^nPara, nPara)
initialSpace[,1] = rep(seq(0.01, 0.05, length.out = 5), nValue^(nPara - 1)) # phi
initialSpace[,2] = rep(rep(seq(2, 22, length.out = 5), each = nValue), nValue^(nPara - 2)) # tau
initialSpace[,3] = rep(seq(0.8, 0.98, length.out = 5), each = nValue^2)

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

save('initialSpace', 'nValue', 'nPara', 'paraNames', 'nComb', 'wInis',
     file = 'outputs/fixInputSimData/initialSpace.RData')

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

