# this script fits parameters using experimental data

# load libraries and scripts
library('nloptr')
source('subFxs/wtwSettings.R')
source('eval_f.R')
source('subFxs/paraFxs.R') # for generate otherPara
source('subFxs/loadFxs.R') # for load data
load('outputs/fixInputSimData/fixInputs.RData')# for wInis
load('outputs/simData/initialSpace.RData')# for nPara
stepDuration = 0.5

# load data
allData = loadAllData()
hdrData = allData$hdrData           
trialData = allData$trialData       
# list with a named element for each subject ID.
allIDs = hdrData$ID                   # column of subject IDs
n = length(allIDs)                    # n
nBlock = 3
cat('Analyzing data for n','=',n,'subjects.\n')

# matrix of starting points 
startPoints = matrix(NA, 3 ^ 3,3)
startPoints[,1] = rep(c(0.05, 0.15, 0.35), each = 3 ^ 2)
startPoints[,2] = rep(c(1, 5, 15), each = 3 , 3)
startPoints[,3] = rep(c(0.75, 0.90, 0.95), 3^2)
stepDuration = 0.5

LLs = vector(length = n )
solutions = matrix(NA, n, nPara)


for(sIdx in 1 : n){
  # extract data
  thisID = allIDs[sIdx]
  thisTrialData = trialData[[thisID]]
  thisTrialData = thisTrialData[thisTrialData$blockNum ==1,]
  # prepare data for para fitting
  thisCond = unique(thisTrialData$condition)
  otherPara = getOtherPara(thisCond, stepDuration)
  wIni = wInis[[thisCond]]
  thisRewardDelays = thisTrialData$scheduledWait
  thisTimeWaited = thisTrialData$timeWaited
  thisTrialEarn = thisTrialData$trialEarnings
  thisTimeWaited[thisTrialEarn == tokenValue] = thisRewardDelays[thisTrialEarn == tokenValue] 
  
  # para fitting
  LL = 1e5
  solution = vector(length = nPara)
  for(s in 1 : nrow(startPoints)){
    x0 = startPoints[s, ]
    local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e6)
    opts = list(algorithm = "NLOPT_LN_BOBYQA", stopval = 5,
                local_optimizer = local_optimizer) 
    
    res = nloptr(x0 = x0, eval_f = eval_f_wait, lb = c(0, 0, 0) , ub = c(1, Inf, 1),
                 opts = opts,
                 otherPara = otherPara, cond = thisCond, wIni = wIni,
                 rewardDelays = thisRewardDelays, trueTimeWaited = thisTimeWaited)
    if(res$objective < LL){
      LL = res$objective
      solution = res$solution
    }
  }# end of loop across starting points
  LLs[sIdx] = LL
  solutions[sIdx,] = solution
}
save('LLs', 'solutions', file = 'outputs/expData/waitRecover.RData')




