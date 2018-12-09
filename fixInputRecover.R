########### load scripts, library ################
# basic
library('dplyr')
library('tidyr')
library('ggplot2')
source('subFxs/plotThemes.R')
library(nloptr)
# resimulate the
source('subFxs/wtwSettings.R') 
source('subFxs/paraFxs.R') 
source('eval_f.R')
load('outputs/fixInputSimData/fixInputs.RData')# load rewardDelays

# need know the true parameter and true outputs
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/rawLPData.RData')



################## compare waitDurations across para
ggplot(colpHPData, aes(dvSucessRecover)) + geom_histogram()
ggplot(colpHPData, aes(waitSucessRecover)) + geom_histogram()

################## recover parameters by MLE ##############
stepDuration = 0.5
cond = 'LP'
otherPara = getOtherPara(cond, stepDuration)
thisRewardDelays = rewardDelays[[cond]]
rawData = rawLPData

combIdx = 20
wIni = wInis[[cond]][combIdx]
para = initialSpace[combIdx, ]
timeWaited = rawData$timeWaited[combIdx, 1, 1 : nTrials ]
paste(sprintf('cond: %s, para: ', cond), round(para[1],2), para[2], para[3])
###### recover
x0 = c(0.1, 5, 0.5)
local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e6)
opts = list(algorithm = "NLOPT_LN_BOBYQA", stopval = 5,
            local_optimizer = local_optimizer) 

res = nloptr(x0 = x0, eval_f = eval_f_wait, lb = c(0, 0, 0) , ub = c(1, Inf, 1),
             opts = opts,
             otherPara = otherPara, cond = cond, wIni = wIni,
             rewardDelays = thisRewardDelays, trueTimeWaited = timeWaited)


####################
vaWaits =  rawData$vaWaits[combIdx, 1, , ]
vaQuits = rawData$vaQuits[combIdx, 1, , ]
trueDvs = transVaWaits(vaWaits) - transVaQuits(vaQuits)

x0 = c(0.1, 5, 0.5)
local_optimizer = list(algorithm = "NLOPT_GN_MLSL_LDS", maxeval = 1e4)
opts = list(algorithm = "NLOPT_LN_BOBYQA", stopval =0.1,
            local_optimizer = local_optimizer) 
res = nloptr(x0 = x0, eval_f = eval_f_dv, lb = c(0, 0, 0) , ub = c(1, Inf, 1),
             opts = opts,
             otherPara = otherPara, cond = cond, wIni = wIni,
             rewardDelays = thisRewardDelays, trueDvs = trueDvs)

