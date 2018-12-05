# this script analyzes the simulation data on the case level

###### load data and functions #######
source('subFxs/helperFxs.R')
library('ggplot2')
source('subFxs/plotThemes.R')
source('subFxs/wtwSettings.R')
source("subFxs/actionValueViewer.R")
load('outputs/QStarData/colpData.RData')
load('outputs/QStarData/initialSpace.RData')
load('outputs/QStarData/RawHPData.RData')
load('outputs/QStarData/RawLPData.RData')

###### choose condition ########3
condIdx = 2
cond = conditions[condIdx]

inputColp = if(cond == 'HP') inputColp = colpHPData else inputColp = colpLPData
inputRaw = if(cond == 'HP') inputRaw = rawHPData else inputRaw= rawLPData

tMax = tMaxs[condIdx]
trialTick = trialTicks[[condIdx]] # so here if use [2] then get a list

####### view simulation data case by case ##########
# choose cases you want to plot
nCombList = which(inputColp$AUC < 6 && inputColp$AUC < 3) 
# choose figrues you want to plot
plotTrialData = T
plotKMSC= T
drawTimeSample = T
plotActionValue = T
# plot
for (nCb in 1 : length(nCombList)){
  i = nCombList[nCb]
  j = 1
  
  # prepare total earnings, wtw and AUC
  totalEarnings = inputColp$totalEarnings[i]
  wtw = inputColp$wtw[i]
  AUC = inputColp$AUC[i]
  label = sprintf('colp stat, earn: %d, wtw: %.2f, AUC: %.2f',
                  totalEarnings, wtw, AUC)
  # block data
  blockData = data.frame(trialEarnings = inputRaw$trialEarnings[i,j,],
                         scheduledWait = inputRaw$rewardDelays[i,j,],
                         timeWaited = inputRaw$timeWaited[i,j,],
                         trialNum = 1 : length(inputRaw$timeWaited[i,j,])
  )
  waitDuration = blockData$timeWaited
  waitDuration[is.na(waitDuration)] = blockData$scheduledWait[is.na(waitDuration)]
  blockData$waitDuration = waitDuration
  endTick = match(0, inputRaw$rewardDelays[i,j,]) - 1
  blockData = blockData[1:endTick, ]
  
  
  if(plotTrialData){
    # plot
    trialPlots(blockData, label)
  }
  
  if(plotTrialData) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
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
    para = initialSpace[i, ]
    vaWaits = inputRaw$vaWaits[i,j, , ]
    vaQuits = inputRaw$vaQuits[i,j, , ]
    actionValueViewer(vaWaits, vaQuits, blockData, para)
  }
  
  if(plotActionValue) {
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
  
}


############ compare with exp data ###########
# load data and functions
source('subFxs/loadFxs.R')
allData = loadAllData()
hdrData = allData$hdrData   
trialData = allData$trialData    
allIDs = hdrData$ID                   # column of subject IDs
load('outputs/expData/groupData.RData')
# choose cases you want to plot 
nCombList = which(groupData$AUC <= 6 & groupData$AUC >= 2 & groupData$condition == cond)
for(nCb in 1 : length(nCombList)){
  idx = nCombList[nCb]
  sIdx = ceiling(idx / 2) 
  bkIdx = idx - (sIdx - 1) * 2
  
  # get data
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  thisCond = unique(thisTrialData$condition)
  thisBlockIdx = (thisTrialData$blockNum == bkIdx)
  thisTrialData = thisTrialData[thisBlockIdx,]
  thisFunction = unique(thisTrialData$trial_function)
  label = sprintf('Subject %s, earn %d)',thisID, thisTrialData$totalEarnings)
  
  tMax = ifelse(thisCond == conditionNames[1], tMaxs[1], tMaxs[2])
  kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
  # plot trialData
  if (plotTrialData) {
    trialPlots(thisTrialData,label)
  }
  
  # survival analysis
  if(plotKMSC){
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
  }

  
  # plot wait time distribution based on survival analysis
  if(drawTimeSample){
    # prepare pdf
    cdf = 1 - kmscResults$kmOnGrid;
    cdf[length(cdf)] = 1
    pdf = diff(c(0, cdf)) # hre 0 is the time tick before 0
    
    # 
    draws = sample(trialTicks$LP, size = 1000, replace = TRUE, prob = pdf)
    p = ggplot(data.frame(draws),aes(draws)) + geom_histogram() + xlim(c(0 - 1, tMax+3)) +
      displayTheme + xlab('Wait duration / s') + ggtitle(label)  + xlab('Wait duration / s')
    print(p)
  }
  
  # # wait for input before continuing, if individual plots were requested
  if(any(plotKMSC, plotTrialData, drawTimeSample)){
    readline(prompt = paste(nCb, '(hit ENTER to continue)'))
  }
}

