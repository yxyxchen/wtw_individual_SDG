# load library 
library(rstan)
Sys.setenv(USE_CXX14=1)
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# model str
cscModel <- "
data {
int N;
real stepDuration;
int nTimeStep;
vector[N] waitDurations;
int<lower = 1, upper = 2> actions[N];

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
vector[nTimeStep, N] vaQuits;
// loop over trials
for(n in 1 : N){
  real endStep = floor(waitDurations[n] / stepDuration);
  // loop over the waiting period
  int t = 1;
  while(t < endStep){
    // record vaWaits and vaQuits
    vaWaits[t, n] = ws[t] * tau;
    vaQuits[t, n] = ws[1] * gamma ^ 4 * tau;
   // update 
    vector[nTimeStep] junk = rep_vector(0, nTimeStep); 
    junk[t] = 1;
    es = es * gamma * lambda + junk;
    // no reward occurs and always waits
    real vaQuit = ws[1] * gamma ^ 4;
    real vaWait= ws[t+1];
    delta = 0 + fmax(vaWait, vaQuit) - ws[t];
    ws = ws + phi * delta * es;
    t = t + 1;
  }
  // when t = endStep
  // record vaWaits and vaQuits
    vaWaits[t, n] = ws[t] * tau;
    vaQuits[t, n] = ws[1] * gamma ^ 4 * tau;
  // update if wait 
  if(actions[n] == 2){
    vector[nTimeStep] junk = rep_vector(0, nTimeStep); 
    junk[t] = 1;
    es = es * gamma * lambda + junk;
    // reward occurs
    delta = tokenValue + fmax(ws[t+1], ws[1] * gamma ^ 4) - ws[t];
    ws = ws + phi * delta * es;
  }
}

}
model {
  phi ~ normal(0.01, 1); 
  // The likelihood
  for(n in 1 : N){
    real endStep = floor(waitDurations[n] / stepDuration);
    int t = 1;
    while(t <= endStep){
      if(t < endStep){
        int action = 2;
      }else{
        int action = 1;
      }
      vector[2] values;
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
  for(n in 1 : N){
    real endStep = floor(waitDurations[n] / stepDuration);
    int t = 1;
    while(t <= endStep){
      if(t < endStep){
      int action = 2;
      }else{
      int action = 1;
      }
      vector[2] values;
      values[1] = vaQuits[t, n];
      values[2] = vaWaits[t, n];
      log_lik[n] =categorical_logit_lpmf(action | values);
      t = t + 1;
    }
  }
} 
"

############## simulate data and run ################
source('subFxs/wtwSettings.R')
source('subFxs/model.R')
source('subFxs/paraFxs.R')
otherPara = getOtherPara(cond, stepDuration)
tMax = ifelse(unique(thisBlockData$condition) == 'HP', tMaxs[1], tMaxs[2])
nTimeStep = tMax / stepDuration
tempt=  simulationModel(para,otherPara, cond)
actions = ifelse(thisBlockData$trialEarnings == 10, 1, 0)
actions = as.integer(actions) + 1;
data_list <- list(N = length(thisBlockData$scheduledWait),
                  waitDurations = thisBlockData$scheduledWait,
                  actions = actions,
                  nTimeStep = nTimeStep,
                  stepDuration = 0.5,
                  tau = 16,
                  gamma = 0.98,
                  lambda = 0.94,
                  wIni = 5)
fit <- stan(model_code = toy_model, data = data_list, cores = 1, chains = 1, iter = 500)
dir.create('outputs/Stan')
fileName = 'outputs/Stan/expFit.RData'
save(fit, file = fileName)