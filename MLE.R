# setting parameters 
stepDuration = 0.5
counts = 
source('subFxs/wtwSettings.R') # wtw settings for both HP and LP
# can't change
source('subFxs/paraFxs.R') 

# 
cond = 'LP'

load('outputs/QStarData/packLPData.RData')
load('outputs/QStarData/initialSpace.RData')
para = initialSpace[combIdx ,]
wIni = wInis[[cond]][combIdx]
otherPara = getOtherPara(cond, stepDuration)

# extract thisPackData
combIdx = 1
j = 1
thisCount = count[combIdx, j];
thisPackData = packLPData[[thisCount]]

# prepare inputs
rewardDelays = thisPackData$rewardDelays;
waitDurations = thisPackData$timeWaited
waitDurations[thisPackData$trialEarnings > 0] =
  rewardDelays[thisPackData$trialEarnings > 0]

# truncate
endStep = match(0, rewardDelays) - 1
waitDurations = waitDurations[1 : endStep]
rewardDelays = rewardDelays[1 : endStep]

# 
source('fittingModel.R')
# vaWaits and vaQuits
vaWaits = thisPackData$vaWaits[,1:endStep]
vaQuits = thisPack$vaQuits[, 1 : endStep]

# if vaWaits is not updated, then just take the value of the last trial
for(i in 2 : endStep){
  if(sum(is.na(vaWaits[,i])) > 0){
    vaWaits[is.na(vaWaits[,i]),i]  = vaWaits[is.na(vaWaits[,i]),i-1] 
  }
}

# if vaQuit is not recorded, it is equal the next recording 
for(i in 1 : endStep){
  if(sum(is.na(vaQuits[,i])) > 0){
    vaQuits[is.na(vaQuits[,i]),i] = vaQuits[match(NA,vaQuits[,i] ) - 1,i]
  }
}

# resimulation using the same para
source('fittingModel.R')
tempt = waitDurationModel(para, otherPara, cond, wIni, rewardDelays)

# extract waitDurations
waitDurationsHat = tempt$timeWaited;
waitDurationsHat[tempt$trialEarnings > 0] = rewardDelays[tempt$trialEarnings > 0]
waitDurationsHat = waitDurationsHat[1 : endStep]

# extract and repaire vaWaits and vaQuits
vaWaitsHat = tempt$vaWaits[,1:endStep]
vaQuitsHat = tempt$vaQuits[,1:endStep]
# if vaWaits is not updated, then just take the value of the last trial
for(i in 2 : endStep){
  if(sum(is.na(vaWaitsHat[,i])) > 0){
    vaWaitsHat[is.na(vaWaitsHat[,i]),i]  = vaWaitsHat[is.na(vaWaitsHat[,i]),i-1] 
  }
}

for(i in 1 : endStep){
  if(sum(is.na(vaQuitsHat[,i])) > 0){
    vaQuitsHat[is.na(vaQuitsHat[,i]),i] = vaQuitsHat[match(NA,vaQuitsHat[,i] ) - 1,i]
  }
}


# compare for the same parameters
mean(abs(waitDurations - waitDurationsHat))
vaDeltas = vaWaits - vaQuits;
vaDeltasHat = vaWaitsHat - vaQuitsHat;
mean(abs(vaDeltas - vaDeltasHat))

# 0,0927

########### resimulate with other para ###########
waitDurationLLs = vector(length = nComb)
vaDeltaLLs = vector(length = nComb)
for(h in 1 : nComb){
  para = initialSpace[h,]
  wIni = wInis[[cond]][h]
  tempt = waitDurationModel(para, otherPara, cond, wIni, rewardDelays)
  
  waitDurationsHat = tempt$timeWaited;
  waitDurationsHat[tempt$trialEarnings > 0] = rewardDelays[tempt$trialEarnings > 0]
  waitDurationsHat = waitDurationsHat[1 : endStep]
  
  # extract and repaire vaWaits and vaQuits
  vaWaitsHat = tempt$vaWaits[,1:endStep]
  vaQuitsHat = tempt$vaQuits[,1:endStep]
  # if vaWaits is not updated, then just take the value of the last trial
  for(i in 2 : endStep){
    if(sum(is.na(vaWaitsHat[,i])) > 0){
      vaWaitsHat[is.na(vaWaitsHat[,i]),i]  = vaWaitsHat[is.na(vaWaitsHat[,i]),i-1] 
    }
  }
  
  for(i in 1 : endStep){
    if(sum(is.na(vaQuitsHat[,i])) > 0){
      vaQuitsHat[is.na(vaQuitsHat[,i]),i] = vaQuitsHat[match(NA,vaQuitsHat[,i] ) - 1,i]
    }
  }
  
  vaDeltasHat = vaWaitsHat - vaQuitsHat;
  
  waitDurationLLs[h] = mean(abs(waitDurations - waitDurationsHat))
  vaDeltaLLs[h] =  mean(abs(vaDeltas - vaDeltasHat))
}


hist(waitDurationLLs)
which.min(waitDurationLLs)
initialSpace[which.min(waitDurationLLs),]
initialSpace[1,]

which.min(vaDeltaLLs)
hist(vaDeltaLLs[vaDeltaLLs < 6], breaks = seq(0, 6, length.out = 10))
mean(vaDeltaLLs)


