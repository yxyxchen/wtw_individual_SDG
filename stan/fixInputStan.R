# set up 
options(warn=-1, message =-1) # default settings borrowed somewhere
library(dplyr); library(ggplot2);library(reshape2); library('rstan'); #load libraries
Sys.setenv(USE_CXX14=1) # making rstan working on this device 
rstan_options(auto_write = TRUE) # default settings borrowed somewhere
options(mc.cores = parallel::detectCores())# enable multi-core precessors 

library('tidyr')


# fitting the model
source('subFxs/wtwSettings.R') 
source('subFxs/fittingFxs.R')
load('outputs/fixInputSimData/initialSpace.RData') # need nComb(for loop), wIni, nPara(for initialize solutions)

# choose the condition
condIdx = 1
cond = conditions[condIdx]
wIni = wInis[[cond]]
tMax = ifelse(cond == 'HP', tMaxs[1], tMaxs[2])
nTimeStep = tMax / stepDuration
if(condIdx == 1){
  load('outputs/fixInputSimData/rawHPData.RData')
  timeWaitedList = rawHPData$timeWaited[,1,]
  trialEarningsList = rawHPData$trialEarnings[,1,]
  rm(rawHPData)
}else{
  load('outputs/fixInputSimData/rawLPData.RData')
  timeWaitedList = rawLPData$timeWaited[,1,]
  trialEarningsList = rawLPData$trialEarnings[,1,]
  rm(rawLPData)
}

# choose the combIdx
combIdx = 1
timeWaited = timeWaitedList[combIdx,];
trialEarnings = trialEarningsList[combIdx,];
nTimePoints = round(ifelse(trialEarnings >0, ceiling(timeWaited / stepDuration), floor(timeWaited / stepDuration) + 1))
data_list <- list(tMax = tMax,
                  wIni = wIni,
                  nTimeStep = nTimeStep,
                  N = length(timeWaited),
                  timeWaited = timeWaited,
                  trialEarnings = trialEarnings,
                  nTimePoints = nTimePoints)
fit <- stan(file = 'stan/model.stan', data = data_list, cores = 1, chains = 2, iter = 500)
dir.create('outputs/Stan')
fileName = 'outputs/Stan/fixInputFitting.RData'
save(fit, file = fileName)

known_parameters <- data_frame(variable = c("phi","tau", "gamma"), real_value = initialSpace[1,])

junk = rstan::extract(fit, permuted = F)

rstan::extract(fit, permuted = F, pars = c("phi", "tau", "gamma", "LL_all")) %>% 
  # extract data
b = adply(a, 2, function(x) x) # change arrays into dataframe 
c = select(b, -chains)
d = gather(c, variable, estimate)
left_join(d, known_parameters, by = "variable")

