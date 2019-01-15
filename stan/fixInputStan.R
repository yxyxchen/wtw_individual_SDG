options(warn=-1, message =-1) # default settings borrowed somewhere
library(dplyr); library(ggplot2);library(reshape2); library('rstan'); #load libraries
library('plyr')
Sys.setenv(USE_CXX14=1) # making rstan working on this device 
rstan_options(auto_write = TRUE) # default settings borrowed somewhere
options(mc.cores = parallel::detectCores())# enable multi-core precessors 
library('tidyr')
source('stan/singleFittingStan.R')
model = stan_model(file = "stan/monte.stan")
pars = c("phi", "tau", "gamma")

# fitting the model
source('subFxs/wtwSettings.R') 
source('subFxs/fittingFxs.R')
load('outputs/fixInputSimData/initialSpace.RData') # need nComb(for loop), wIni, nPara(for initialize solutions)

for(condIdx in 1:2){
  # choose the condition
  cond = conditions[condIdx]
  wIni = wInis[[cond]]
  tMax = ifelse(cond == 'HP', tMaxs[1], tMaxs[2])
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
  for(combIdx in 1: nPara){
    # choose the combIdx
    timeWaited = timeWaitedList[combIdx,];
    trialEarnings = trialEarningsList[combIdx,];
    fileName = sprintf("stanOutPuts/fixInputSimData/%s_$d.txt", cond, combIdx)
    singleFittingStan(combIdx, cond, wIni, timeWaited, trialEarnings, fileName, pars)
  }
}
