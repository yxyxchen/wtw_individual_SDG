# this script fits parameters using experimental data

# load libraries and scripts
library('nloptr')
library('parallel')
source('subFxs/wtwSettings.R')
source('subFxs/fittingFxs.R')
source('subFxs/loadFxs.R') # for load data
load('outputs/simData/initialSpace.RData')# for wInis

# load data
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData       
# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n

trialData = trialData[(1 : length(trialData)) %in% allIDs]
timeWaitedList = sapply(1 :n, function(sIdx) {
  tempt = trialData[[sIdx]]
  junk = tempt$timeWaited[1 : (sum(tempt$blockNum == 1) - 1)]
})
trialEarningsList = sapply(1 :n, function(sIdx) {
  tempt = trialData[[sIdx]]
  junk = tempt$trialEarnings[1 : (sum(tempt$blockNum == 1) - 1)]
})
scheduledWaitList = sapply(1 :n, function(sIdx) {
  tempt = trialData[[sIdx]]
  junk = tempt$scheduledWait[1 : (sum(tempt$blockNum == 1) - 1)]
})
condList = sapply(1 :n, function(sIdx) {
  tempt = trialData[[sIdx]]
  junk = unique(tempt$condition)
})
wIniList = ifelse(condList == 'HP', wInis[1], wInis[2])
timeWaitedList = sapply(1:n, function(sIdx){
  ifelse(trialEarningsList[[sIdx]] > 0, scheduledWaitList[[sIdx]], timeWaitedList[[sIdx]])
})

rm(allData)
rm(trialData)
rm(hdrData)


# matrix of starting points 
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.1, 0.5, 0.9), each = 3 ^ 2)
startPoints[,2] = rep(c(2, 15, 28), each = 3 , 3)
startPoints[,3] = rep(c(0.1, 0.5, 0.90), 3^2)

negLLs = vector(length = n )
solutions = matrix(NA, n, nPara)

tempt = mclapply(1 : n, function(sIdx)
  singleFitting(sIdx, condList[[sIdx]], wIniList[[sIdx]],
                trialEarningsList[[sIdx]], timeWaitedList[[sIdx]], startPoints), mc.cores = 2)
junk = matrix(unlist(tempt), nrow = n, byrow = T)
negLLs = junk[,1]
solutions = junk[,2:4]
save('negLLs', 'solutions', file = 'outputs/expData/actionRecover.RData')

x0 = startPoints[1,]
cond = condList[[sIdx]]
wIni = wIniList[[sIdx]]
trialEarnings = trialEarningsList[[sIdx]]
timeWaited = timeWaitedList[[sIdx]]
negLLAction(x0, cond, wIni, trialEarnings, timeWaited)

