# this script analyzes the simulation data on the case level

###### load data and functions #######
source('subFxs/helperFxs.R')
library('ggplot2')
source('subFxs/plotThemes.R')
source('subFxs/wtwSettings.R')
source("subFxs/actionValueViewer.R")
source("model.R")
source("subFxs/paraFxs.R")
###### choose condition ########3
condIdx = 2
cond = conditions[condIdx]
tMax = tMaxs[condIdx]
trialTick = trialTicks[[condIdx]] # so here if use [2] then get a list

# simulate 
stepDuration = 0.5
otherPara = getOtherPara(cond, stepDuration)

set.seed(123)
para = c(0.1, 8, 0.98)
tempt = simulationModel(para,otherPara, cond, 3)

# preprocess
waitDuration = tempt$timeWaited
rewardDelay = tempt$rewardDelays
quitIdx = (tempt$trialEarnings == 0)

waitDuration[is.na(waitDuration)] = rewardDelay[is.na(waitDuration)]
endTick = match(0,rewardDelay)
waitDuration = waitDuration[1 : (endTick - 1)]
quitIdx = quitIdx[1 : (endTick - 1)]

kmscResults = kmscSimple(waitDuration, quitIdx, tMax, trialTick)
wtwResults = wtwTSSimple(waitDuration, quitIdx, tGrid, tMax)

####### view simulation data case by case ##########
# choose figrues you want to plot
plotTrialData = T
plotKMSC= T
drawTimeSample = T
plotActionValue = T
# plot

  
# prepare total earnings, wtw and AUC
totalEarnings = sum(tempt$trialEarnings)
AUC = kmscResults$auc
label = sprintf('earn: %d, AUC: %.2f',
                totalEarnings, AUC)
# block data
blockData = data.frame(trialEarnings = tempt$trialEarnings,
                       scheduledWait = tempt$rewardDelays,
                       timeWaited = tempt$timeWaited,
                       trialNum = 1 : length(tempt$trialEarnings)
)
waitDuration = blockData$timeWaited
waitDuration[is.na(waitDuration)] = blockData$scheduledWait[is.na(waitDuration)]
blockData$waitDuration = waitDuration
endTick = match(0, tempt$rewardDelays) - 1
blockData = blockData[1:endTick, ]


  if(plotTrialData){
    # plot
    trialPlots(blockData, label)
  }
  
  
  
  # look at kmsc
  if(plotKMSC){
    # change name
    rewardDelay = blockData$scheduledWait
    quitIdx = (blockData$trialEarnings == 0)
    
    
    kmscResults = kmscSimple(blockData$waitDuration, quitIdx, tMax, trialTick)
    plotData = data.frame(pSurvival = kmscResults$kmOnGrid, time = trialTicks[[cond]])
    p = ggplot(plotData, aes(time, pSurvival)) + geom_line() + ylim(c(0, 1)) + displayTheme +
      ggtitle(label)
    print(p)
  }
  
  if(plotKMSC) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
  # draw wait duration distribution
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    draws = sample(trialTicks[[cond]], size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram(bins = 50) + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)
    print(p)
  }
  
  if(drawTimeSample) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
  # plot action value
  # prepare data 
  if(plotActionValue){
    vaWaits = tempt$vaWaits
    vaQuits = tempt$vaQuits
    actionValueViewer(vaWaits, vaQuits, blockData, para)
  }
  
  if(plotActionValue) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
