# this script analyze the experimental data 

########### load data and functions ##########
# functions 
source('subFxs/loadFxs.R')
source('subFxs/helperFxs.R')
source("subFxs/plotThemes.R")
source("subFxs/wtwSettings.R")
library("ggplot2")
library('dplyr')
library(Hmisc)

# load all data
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData       
# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
nBlock = 3
cat('Analyzing data for n','=',n,'subjects.\n')

# 
# control which individual-level plots to generate
plotTrialwiseData = F
plotKMSC = F
plotWTW = F
plotTimeEarnings = F
plotTrialEarnings =  F


# initialize outputs, organised by block
tGrid = seq(0, blockSecs, by = 0.1)
grpAUC = numeric(length =n * nBlock)
earningsByBlock = numeric(length= n * nBlock)
condByBlock = vector(length= n * nBlock)
stressByBlock = vector(length= n * nBlock)
wtw = matrix(NA, length(tGrid), n * nBlock)
cumEarn = matrix(NA, length(tGrid), n * nBlock)

# reaction time analysis 
for(sIdx in 1 : n){
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  condition = unique(thisTrialData$condition)
  thisTrialData = thisTrialData[thisTrialData$blockNum == 2, ]
  thisRT = thisTrialData$timeWaited - thisTrialData$rewardTime;
  thisRewardDelays = thisTrialData$scheduledWait
  plotData = data.frame(RT = thisRT[!is.nan(thisRT)],
                        rewardDelay = thisRewardDelays[!is.nan(thisRT)])
  
  ggplot(plotData, aes(rewardDelay, RT)) + geom_point()
}
# descriptive statistics for individual subjects and blocks
for (sIdx in 1:n) {
  thisID = allIDs[sIdx]
  for (bkIdx in 1:nBlock){
    # pull this subject's data
    thisTrialData = trialData[[thisID]]
    thisCond = unique(thisTrialData$condition)
    thisBlockIdx = (thisTrialData$blockNum == bkIdx)
    thisTrialData = thisTrialData[thisBlockIdx,]
    thisStress = unique(thisTrialData$stress)
    label = sprintf('Subject %s, Cond %s, Stress %s)',thisID, thisCond, thisStress)
    noIdx = (sIdx - 1) * nBlock + bkIdx
      
    # 
    tMax = ifelse(thisCond == condition[1], tMaxs[1], tMaxs[2])
    kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.

    #  summarise blockwise conditions and response functions
    condByBlock[noIdx] = thisCond
    stressByBlock[noIdx] = thisStress
    earningsByBlock[noIdx] =  sum(thisTrialData$trialEarnings)
    
    # plot trial-by-trial data
    if (plotTrialwiseData) {
      trialPlots(thisTrialData,label)
    }
    
    # survival analysis
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
    grpAUC[noIdx] = kmscResults[['auc']]


    # WTW time series
    wtwCeiling = tMax
    wtwtsResults = wtwTS(thisTrialData, tGrid, wtwCeiling, label, plotWTW)
    wtw[, noIdx] =   wtwtsResults 

    # accumutative timeEarnings 
    timeEarnings = getTimeEarnings(thisTrialData, tGrid, label, plotTimeEarnings)
    cumEarn[,noIdx] = timeEarnings
    
    # accumutative trialEarnings 
    plotData = data.frame(trialNum = thisTrialData$trialNum,
                          cumEarnings = cumsum(thisTrialData$trialEarnings))
    if(plotTrialEarnings){
      p = ggplot(plotData, aes(trialNum, cumEarnings)) + geom_line()   
      print(p)
    }
    
    
    # wait for input before continuing, if individual plots were requested
    if (any(plotTrialwiseData, plotKMSC, plotWTW, plotTimeEarnings, plotTrialEarnings)) {
      readline(prompt = paste('subject',thisID, "block", bkIdx, '(hit ENTER to continue)'))
      graphics.off()
    }
  } # loop over blocks
}

# organize and save groupdata 
groupData = data.frame(id = rep(allIDs, each = nBlock), blockNum = rep( t(1 : nBlock), n),
                       cbal = rep(hdrData$cbal, each = nBlock), condition = factor(condByBlock, levels = c('HP', 'LP')),
                       stress = stressByBlock, AUC = grpAUC, 
                       totalEarnings = earningsByBlock)
dir.create('outputs/expData')
save(groupData, file = 'outputs/expData/groupData.RData')

##### plot total earnings
ggplot(groupData, aes(totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of blocks") + saveTheme + xlim(c(0, 600))
ggsave("outputs/exp_figures/earningExp.pdf", width = 8, height = 4)

### plot AUC distribution 
plotData = data.frame(AUC = groupData$AUC, totalEarnings = groupData$totalEarnings,
                      condition = groupData$condition, blockNum = factor(groupData$blockNum ))
mean_se =  function(x) {
  m <- mean(x)
  se = sd(x) / sqrt(length(x))
  ymin <- m-se
  ymax <- m+se
  return(c(y=m,ymin=ymin,ymax=ymax))
}
p = ggplot(plotData, aes(blockNum, AUC, fill = condition, color = condition)) +
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(0.6), stackratio= 1, dotsize=0.5) +
  scale_fill_manual(values= conditionColors) + scale_color_manual(values= conditionColors) +
  stat_summary(fun.data= mean_se, geom="pointrange", position=position_dodge(0.6), color = 'red', size = 0.5, shape = 4)
p + saveTheme + xlab('Block No.') + ylab('AUC / s') 
ggsave("outputs/exp_figures/auc_distribution.pdf", width = 8, height = 4)


#### 
