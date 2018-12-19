library('ggplot2')
source('model.R')
source('subFxs/loadFxs.R')
source("subFxs/plotThemes.R")
source("subFxs/wtwSettings.R")
source('suBFxs/paraFxs.R')

# load all data
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData   

# select data
thisTrialData = trialData[[1]]
bkIdx = 1
thisBlockData = thisTrialData[thisTrialData$blockNum == bkIdx,]

# prepare arguments 
thisCond = ifelse(unique(thisBlockData$condition) == 'HP', conditions[1], conditions[2])

waitDurations = thisBlockData$timeWaited
waitDurations[thisBlockData$trialEarnings == 5] = thisBlockData$scheduledWait[thisBlockData$trialEarnings == 5]
nTrial = nrow(thisBlockData)
inputData = list(nTrial = nTrial,
                 rewardDelays = thisBlockData$scheduledWait,
                 trialEarnings = thisBlockData$trialEarnings,
                 waitDurations = waitDurations
                 )
stepDuration = 0.5
otherPara = getOtherPara(thisCond, stepDuration)

# simulate 
para = c(0.02, 16, 0.98, 0.94, 8)
tempt = fittingModel(para, otherPara, inputData)

# fitting
both = rep(c(NA), nTrial)
both[(thisBlockData$trialEarnings + tempt$trialEarnings)== 20] = 10
both[(thisBlockData$trialEarnings + tempt$trialEarnings)== 0] = 0
plotData = data.frame(trial = rep(1 : nTrial, 3),
                      trialEarings = c(thisBlockData$trialEarnings,tempt$trialEarnings,both),
                      
                      condition = rep(c('experiment', 'prediction', 'both'), each = nTrial))

ggplot(plotData, aes(trial, trialEarings, color = condition)) + geom_point() 

