# load library 
library(rstan)
Sys.setenv(USE_CXX14=1)
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


############## simulate data and run ################
load('outputs/QStarData/colpData.RData')
load('outputs/QStarData/initialSpace.RData')
load('outputs/QStarData/RawHPData.RData')
load('outputs/QStarData/RawLPData.RData')
source('subFxs/wtwSettings.R')

###### choose condition ########3
condIdx = 1
cond = conditions[condIdx]
condName = conditionNames[condIdx]

inputColp = if(condName == 'HP') inputColp = colpHPData else inputColp = colpLPData
inputRaw = if(condName == 'HP') inputRaw = rawHPData else inputRaw= rawLPData

tMax = tMaxs[condIdx]
stepDuration = 0.5
nTimeStep = tMax / stepDuration

# comb index 
cbIdx = 1;

# learning para 
para = initialSpace[1,]
phi = para[1]
tau =  para[2]
gamma =  para[3]
lambda =  para[4]
wIni =  para[5]

# data 
trialEarnings = inputRaw$trialEarnings[cbIdx, 1, ]
timeWaited = inputRaw$timeWaited[cbIdx, 1, ]
rewardDelays = inputRaw$rewardDelays[cbIdx, 1, ]

# nTrial
nTrial = match(0, rewardDelays) - 1
# binary trialEarnings
rewards = ifelse(trialEarnings[1 : nTrial] == tokenValue, 1, 0)
# generate the action matrix
actions = matrix(2, nTimeStep, nTrial)
waitDurations = timeWaited
waitDurations[trialEarnings == tokenValue] = rewardDelays[trialEarnings == tokenValue]
waitDurations = waitDurations[1 : nTrial]
for(n in 1 : nTrial){
  # if rewarded, action = wait
  endTick =  floor(waitDurations[n] / stepDuration + rewards[n]);
  # if rewarded,e.g 4.33 step, wait at 4 step
  # if not, it will be int, e.g. wait for 5 step, it means quit at 6 step
  actions[endTick,n] = ifelse(rewards[n], 2, 1)
  actions[(endTick + 1) : nTimeStep, n ] = NA
}
rm(rawHPData)
rm(rawLPData)



############ run 
data_list <- list(N = nTrial,
                  rewards = rewards,
                  waitDurations = waitDurations,
                  nTimeStep = nTimeStep,
                  stepDuration = stepDuration,
                  tau = tau,
                  gamma = gamma,
                  lambda = lambda,
                  wIni = wIni)

#############
# model str
cscModel <- "
data {
int N;
real stepDuration;
int nTimeStep;
int rewards[N];
vector[N] waitDurations;
// int<lower = 1, upper = 2> actions[nTimeStep, N];

// fixed learning paras
real tau;
real gamma;
real lambda;
real wIni;
}
parameters {
real<lower = 0, upper = 1> phi;
}
transformed parameters{
// initialize 
vector[nTimeStep] ws = rep_vector(wIni, nTimeStep); 
vector[nTimeStep] es = rep_vector(0, nTimeStep); 
matrix[nTimeStep, N] vaWaits;
matrix[nTimeStep, N] vaQuits;

// loop over trials
for(n in 1 : N){
  // 
  real endStep = floor(waitDurations[n] / stepDuration + rewards[n]);

  // 
  vector[nTimeStep] junk;
  real vaQuit;
  real vaWait;
  real delta;
  // loop over the waiting period
  int t = 1;
  while(t <= endStep){
  // record vaWaits and vaQuits
  vaWaits[t, n] = ws[t] * tau;
  vaQuits[t, n] = ws[1] * gamma ^ 4 * tau;
  // update 
  junk = rep_vector(0, nTimeStep); 
  junk[t] = 1;
  es = es * gamma * lambda + junk;
  // no reward occurs and always waits
  vaQuit = ws[1] * gamma ^ 4;
  vaWait= ws[t+1];
  // no update if quits (given endStep difination, for simulation data, no reward = quit)
    if( t < endStep || rewards[n]){
      delta = rewards[n] + fmax(vaWait, vaQuit) - ws[t];
      ws = ws + phi * delta * es;
      t = t + 1;
    }
  }
}

}
model {
phi ~ normal(0.01, 1); 
// The likelihood
for(n in 1 : N){
real endStep = floor(waitDurations[n] / stepDuration + rewards[n]);
int t = 1;
vector[2] values;
int action;
while(t <= endStep){
if(t == endStep && rewards[n] == 0){
  action = 1;
}else{
  action = 2;
}
values[1] = vaQuits[t, n];
values[2] = vaWaits[t, n];
action ~ categorical_logit(values);
t = t + 1;
}
}
}
generated quantities {
// For model comparison, we'll want to keep the likelihood contribution of each point
vector[N] log_lik;
int actions[nTimeStep, N] ;
int action;
for(n in 1 : N){
  real endStep = floor(waitDurations[n] / stepDuration + rewards[n]);
  int t = 1;
  vector[2] values;
  while(t <= endStep){
    if(t == endStep && rewards[n] == 0){
      action = 1;
    }else{
     action = 2;
    }
    values[1] = vaQuits[t, n];
    values[2] = vaWaits[t, n];
    log_lik[n] =categorical_logit_lpmf(action | values);
    actions[t,n] = action;
    t = t + 1;
  }
}
} 
"
fit <- stan(model_code = cscModel, data = data_list, cores = 1, chains = 1, iter = 500)
fileName = 'outputs/Stan/cscFit.RData'
save(fit, file = fileName)