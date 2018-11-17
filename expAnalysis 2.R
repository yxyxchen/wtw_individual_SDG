# this script analyze the experimental data 

########### load data and functions ##########
# functions 
source('subFxs/loadFxs.R')
source('subFxs/helperFxs.R')
source("subFxs/plotThemes.R")
source("subFxs/wtwSettings.R")
library("ggplot2")
library('dplyr')

# load all data
allData = loadAllData()
hdrData = allData$hdrData             # unpack header data
# data frame with variables "ID", "Cbal", "Condition1", "Condition2"
trialData = allData$trialData         # unpack trial data
# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
cat('Analyzing data for n','=',n,'subjects.\n')

# 
# control which individual-level plots to generate
plotTrialwiseData = F
plotKMSC = F
plotWTW = F
plotTimeEarnings = F
plotTrialEarnings =  F



# initialize outputs, organised by block
grpAUC = numeric(length =n * 2)
earningsByBlock = numeric(length= n * 2)
condByBlock = vector(length= n * 2)
FunctionByBlock = vector(length= n * 2)
wtw = matrix(NA, 9001, n * 2)
cumEarn = matrix(NA, 9001, n * 2)


# descriptive statistics for individual subjects and blocks
for (sIdx in 1:n) {
  thisID = allIDs[sIdx]
  for (bkIdx in 1:2){
    # pull this subject's data
    thisTrialData = trialData[[thisID]]
    thisCond = unique(thisTrialData$condition)
    thisBlockIdx = (thisTrialData$blockNum == bkIdx)
    thisTrialData = thisTrialData[thisBlockIdx,]
    thisFunction = unique(thisTrialData$trial_function)
    label = sprintf('Subject %s, Cond %s, Fun %s)',thisID, thisCond, thisFunction)
    
    # 
    tMax = ifelse(thisCond == conditionNames[1], tMaxs[1], tMaxs[2])
    kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
    tGrid = seq(0, blockSecs, by = 0.1)
    
    #  summarise blockwise conditions and response functions
    if(bkIdx == 1){condByBlock[sIdx*2 - 1] = thisCond}
    if(bkIdx == 2){condByBlock[sIdx*2] = thisCond}
    if(bkIdx == 1){FunctionByBlock[sIdx*2 - 1] = thisFunction}
    if(bkIdx == 2){FunctionByBlock[sIdx*2] = thisFunction}
    
    # summarise blockwise earnings 
    if(bkIdx == 1){earningsByBlock[sIdx*2 - 1] = sum(thisTrialData$trialEarnings)}
    if(bkIdx == 2){earningsByBlock[sIdx*2] = sum(thisTrialData$trialEarnings)}
    
    # plot trial-by-trial data
    if (plotTrialwiseData) {
      trialPlots(thisTrialData,label)
    }
    if (plotTrialwiseData) {
      readline(prompt = paste('subject',thisID, "block", bkIdx, '(hit ENTER to continue)'))
      graphics.off()
    }
    
    # survival analysis
    kmscResults = kmsc(thisTrialData,tMax,label,plotKMSC,kmGrid)
    if(bkIdx == 1){grpAUC[sIdx*2 -1] = kmscResults[['auc']]}
    if(bkIdx == 2){grpAUC[sIdx*2] = kmscResults[['auc']]}

    # WTW time series
    wtwCeiling = tMax
    wtwtsResults = wtwTS(thisTrialData, tGrid, wtwCeiling, label, plotWTW)
    if(bkIdx == 1) wtw[,sIdx * 2 -1] = wtwtsResults else wtw[,sIdx * 2] = wtwtsResults

    # accumutative timeEarnings 
    timeEarnings = getTimeEarnings(thisTrialData, tGrid, label, plotTimeEarnings)
    if(bkIdx == 1) cumEarn[,sIdx * 2 -1] = timeEarnings else cumEarn[,sIdx * 2] = timeEarnings
    
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
groupData = data.frame(id = rep(allIDs, each = 2), blockNum = rep(c(1,2), n),
                       cbal = rep(hdrData$Cbal, each = 2), condition = factor(condByBlock, levels = c('HP', 'LP')),
                       trialFun = FunctionByBlock, AUC = grpAUC,
                       totalEarnings = earningsByBlock)
save(groupData, file = 'outputs/expData/groupData.RData')


### plot wtw
meanValues = c(apply(wtw[,groupData$condition == 'HP'], MARGIN = 1, FUN = mean), 
               apply(wtw[,groupData$condition == 'LP'], MARGIN = 1, FUN = mean))
stdValues = c(apply(wtw[,groupData$condition == 'HP'], MARGIN = 1, FUN = sd), 
               apply(wtw[,groupData$condition == 'LP'], MARGIN = 1, FUN = sd))
plotData = data.frame(meanValues, stdValues,
                      time = rep(tGrid, time = 2),
                      condition = rep(c('HP', 'LP'), each = length(tGrid)),
                      minValues = meanValues - stdValues / sqrt(ncol(wtw) / 2),
                      maxValues = meanValues + stdValues / sqrt(ncol(wtw) / 2))

ggplot(plotData, aes(time, meanValues)) +
  geom_ribbon(aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
  geom_line(aes(color = condition), size = 1) + facet_wrap(~condition) + 
  xlab('Time in block / s') + ylab('WTW / s') + saveTheme + scale_color_manual(values=conditionColors)

ggsave("outputs/exp_figures/wtwTimeseries.pdf", width = 12, height = 8)


######  plot AUC against earnings in LP ########
earningsLP = groupData$totalEarnings[groupData$condition == 'LP']
AUCLP = groupData$AUC[groupData$condition == 'LP']
plotData = data.frame(totalEarnings = earningsLP, AUC = AUCLP)
plotData$binary = cut(plotData$AUC, c(0, 15, 32))
plotData$cate = cut(plotData$AUC, seq(0, 32, by = 2))
tempt = summarise(group_by(plotData, cate), meanValues = mean(totalEarnings),
                     stdValues = sd(totalEarnings))
tempt$cateMean = which( levels(tempt$cate) %in% tempt$cate) * 2 - 1
leftCateMean = which( !levels(tempt$cate) %in% tempt$cate) * 2 - 1
plotData2 = data.frame(meanValues = c(tempt$meanValues, rep(350, length(leftCateMean))),
                       stdValues = c(tempt$stdValues, rep(NA, length(leftCateMean))),
                       cateMean = c(tempt$cateMean,leftCateMean),
                       condition = c(rep('data', length(tempt$cateMean)),
                                    rep('missing', length(leftCateMean))))

plotData2$minValues = plotData2$meanValues - plotData2$stdValues
plotData2$maxValues = plotData2$meanValues + plotData2$stdValues
ggplot(plotData2, aes(cateMean, meanValues, color = condition)) + geom_point() +
  ylab('Total earnings') + xlab('AUC interval / s') + 
  geom_errorbar(aes(ymax= maxValues, ymin = minValues)) + 
  displayTheme + theme(axis.text.x = element_text(angle = 45)) + 
  scale_color_manual(values = c('black', 'red')) +
  geom_vline(xintercept = timings$LP, linetype = 2, color = "#404040")
outFile = 'outputs/exp_figures'
fileName = file.path(outFile, "AUCLP_earningsLP_bin.pdf") 
ggsave(fileName, width = 6, height = 4)


##### plot total earnings
ggplot(groupData, aes(totalEarnings)) + geom_histogram(bins = 10) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of blocks") + saveTheme + xlim(c(0, 600))
ggsave("outputs/exp_figures/earningExp.pdf", width = 8, height = 4)


#### plot AUC and total earnings in LP
plotData = data.frame(AUC = groupData$AUC, totalEarnings = groupData$totalEarnings,
                      condition = groupData$condition)
ggplot(plotData[plotData$condition == 'LP',], aes(AUC, totalEarnings)) + geom_point(size = 1.5) + saveTheme+
  xlab('AUC /s') + ylab('Total earnings') + xlim(c(0, tMaxs[2])) + ylim(c(0, 500))
ggsave('outputs/exp_figures/AUCLP_earningsLP.pdf', width = 6, height = 4)


plotData = data.frame(AUC = groupData$AUC, totalEarnings = groupData$totalEarnings,
                      condition = groupData$condition)
ggplot(plotData[plotData$condition == 'HP',], aes(AUC, totalEarnings)) + geom_point(size = 1.5) + saveTheme+
  xlab('AUC /s') + ylab('Total earnings') + xlim(c(0, tMaxs[1]))+ ylim(c(0, 500))
ggsave('outputs/exp_figures/AUCHP_earningsHP.pdf', width = 6, height = 4)