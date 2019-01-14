# load library 
library(rstan)
Sys.setenv(USE_CXX14=1)
options(warn=-1, message =-1)
library(dplyr); library(ggplot2); library(rstan); library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# input
sigma = 1
stepDuration = 0.5
waitRate = 0.9
rewardDelays = thisBlockData$scheduledWait

# toy stan model 
test_string = "
functions {
  vector action_rng(int nTimeStep, real stepDuration, vector rewardDelays, vector actions, vector waitDurations) {
    // learning para
    real phi = 0.02;
    real tau = 16;
    real gamma = 0.98;
    real lambda = 0.94;
    real wIni = 8;
    
    vector[num_elements(rewardDelays)] predictions;
  
    real tokenValue = 10
  
    real endStep;
    real rewardDelay;
    vector[nTimeStep] ws = rep_row_vector(wIni, nTimeStep);
    vector[nTimeStep] es = rep_row_vector(0, nTimeStep);
    int t;
    
    for(n in 1 : num_elements(rewardDelays)){
      rewardDelay  = rewardDelays[n];
      endStep = floor(rewardDelay / stepDuration);
      
      t = -1;
      // wait duration
      while(t < endStep){
        t = t + 1;
        vector[nTimeStep] junk = rep_row_vector(0, nTimeStep);
        junk[t] = 1
        es =  gamma * lambda * es + junk
    
        delta = 0 + c(gamma * max(ws[t+1], ws[1] * gamma ^ 4)) - ws[t]
        ws = ws + phi * delta * es
       }
      
        // if wait then learn 
        if(actions[n] == 1){
          junk[t] = 1
          es =  gamma * lambda * es + junk
          delta = tokenValue + c(gamma * max(ws[t+1], ws[1] * gamma ^ 4)) - ws[t]
          ws = ws + phi * delta * es
        }
        predictions[n] = 
      } // end of while
    
    }// end of function
}
data {
}
parameters {
}
model {
}
"
compiled_function <- stan_model(model_code = test_string) # you could use file = "path/to/yourfile.stan" if you have saved it as so
# And make the function available to the user in R
expose_stan_functions(compiled_function)

action_rng(0.9, 0.5, thisBlockData$scheduledWait, 1)

#######################################

# fixed_ratio_wait
toy_model <- "
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
  vector[N] vaWaits;
  vector[N] vaQuits;
  real tokenValue = 10;
  real delta;
  // loop over trials
  for(n in 1 : N){
    real endStep = floor(waitDurations[n] / stepDuration);
    // loop over the waiting period
    int t = 1;
    while(t < endStep){
      vector[nTimeStep] junk = rep_vector(0, nTimeStep); 
      junk[t] = 1;
      es = es * gamma * lambda + junk;
      // no reward occurs
      delta = 0 + fmax(ws[t+1], ws[1] * gamma ^ 4) - ws[t];
      ws = ws + phi * delta * es;
      t = t + 1;
    }
    // when t = endStep
    vaWaits[n] = ws[t] * tau;
    vaQuits[n] = ws[1] * gamma ^ 4 * tau;
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
    int action = actions[n];
    vector[2] values;
    values[1] = vaQuits[n];
    values[2] = vaWaits[n];
    action ~ categorical_logit(values);
  }
}
generated quantities {
  // For model comparison, we'll want to keep the likelihood contribution of each point
  vector[N] log_lik;
  real      LL_all;
  for (n in 1:N){
    int action = actions[n];
    vector[2] values;
    values[1] = vaQuits[n];
    values[2] = vaWaits[n];
    log_lik[n] =categorical_logit_lpmf(action | values);
  }
    LL_all=sum(log_lik);
} 
"

tMax = ifelse(unique(thisBlockData$condition) == 'HP', tMaxs[1], tMaxs[2])
nTimeStep = tMax / stepDuration
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