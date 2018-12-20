# load data
source('subFxs/loadFxs.R')
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData       
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
nBlock = 3
cat('Analyzing data for n','=',n,'subjects.\n')

# parameters 
para = c(0.2, 10, 0.90)


# load scripts, libraries and data for simulation 
source('model.R') 
source('subFxs/wtwSettings.R')
source('subFxs/paraFxs.R')
source('subFxs/helperFxs.R')
load('outputs/simData/initialSpace.RData') # for wInis

############## simulation ###########
# set seed
set.seed(123)
# initialize output
nRep = 5
count = t(matrix(1 : (n * nRep), nRep, n))
packData = vector(length = n * nRep, mode ='list')
aucData = matrix(NA, n, nRep)

for(sIdx in 1 : n){
  # extract data 
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  thisTrialData = thisTrialData[thisTrialData$blockNum == 1, ]
  thisCond = unique(thisTrialData$condition)
  thisRewardDelays = thisTrialData$scheduledWait
  wIni = wInis[[thisCond]]
  
  # prepare additional data
  otherPara = getOtherPara(thisCond, 0.5)
  tMax = ifelse(thisCond == conditions[1], tMaxs[1], tMaxs[2])
  kmGrid = seq(0, tMax, by=0.1) # grid on which to average survival curves.
  for(j in 1 : nRep){
    tempt=  seqModel(para,otherPara, thisCond, wIni, thisRewardDelays)
    packData[[count[sIdx, j]]] = tempt    
    kmscResults = kmscSimple(tempt$timeWaited, tempt$trialEarnings == 0, tMax, kmGrid)
    aucData[sIdx, j] = kmscResults$auc
  }
} 

aucDataMu = rowSums(aucData) / ncol(aucData)
##### 
load('outputs/expData/groupData.RData')
groupData = groupData[groupData$blockNum == 1,]
aucHP = groupData$AUC[groupData$condition == 'HP']
aucLP = groupData$AUC[ groupData$condition == 'LP']

aucHPSim = aucDataMu[groupData$condition == 'HP']
aucLPSim = aucDataMu[groupData$condition == 'LP']


cor.test(aucHP, aucHPSim)
cor.test(aucLP, aucLPSim)

###### plot
library('ggplot2')
source('subFxs/plotThemes.R')
plotData = data.frame(auc = c(aucHP, aucLP), aucSim = c(aucHPSim, aucLPSim),
                      condition = rep(c('HP', 'LP'), each = length(aucHP)))

# HP
corResults= cor.test(aucHP, aucHPSim)
ggplot(plotData[plotData$condition == 'HP', ], aes(aucSim, auc)) + geom_point() + saveTheme +
  ggtitle('HP') + geom_smooth(method = 'lm') +
  annotate("text", x = 14, y = 10,
           label = sprintf('%.2f(p = %.2f)', corResults$estimate, corResults$p.value),
           size = 6, color = 'blue')
dir.create('outputs/seq_figures')
ggsave('outputs/seq_figures/HP.pdf', width = 6, height = 4)


# LP
corResults= cor.test(aucLP, aucLPSim)
ggplot(plotData[plotData$condition == 'LP', ], aes(aucSim, auc)) + geom_point() + saveTheme +
  ggtitle('LP') + geom_smooth(method = 'lm') +
  annotate("text", x = 8, y = 30,
           label = sprintf('%.2f(p = %.2f)', corResults$estimate, corResults$p.value),
           size = 6, color = 'blue')
ggsave('outputs/seq_figures/LP.pdf', width = 6, height = 4)


