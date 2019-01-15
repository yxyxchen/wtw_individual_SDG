# this script fits parameters using experimental data
options(warn=-1, message =-1) # default settings borrowed somewhere
library(dplyr); library(ggplot2);library(reshape2); library('rstan'); #load libraries
library('plyr')
Sys.setenv(USE_CXX14=1) # making rstan working on this device 
rstan_options(auto_write = TRUE) # default settings borrowed somewhere
options(mc.cores = parallel::detectCores())# enable multi-core precessors 
library('tidyr')
source('stan/singleFittingStan.R')
model = stan_model(file = "stan/monteRatio.stan")
pars = c("phi", "tau", "gamma", "ratio")

# fitting the model
source('subFxs/wtwSettings.R') 
source('subFxs/fittingFxs.R')
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
wIniList = ifelse(condList == "HP", wInis[1], wInis[2])
timeWaitedList = sapply(1:n, function(sIdx){
  ifelse(trialEarningsList[[sIdx]] > 0, scheduledWaitList[[sIdx]], timeWaitedList[[sIdx]])
})

for(sIdx in 1 : n){
  wIni = wIniList[[sIdx]]
  cond = condList[[sIdx]]
  trialEarnings= trialEarningsList[[sIdx]]
  timeWaited = timeWaitedList[[sIdx]]
  fileName = sprintf("stanOutPuts/monteRatio/%d.txt", sIdx)
  singleFittingStan(sIdx, cond, wIni, timeWaited, trialEarnings, fileName, pars)
}