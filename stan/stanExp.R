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
  vector action_rng(real waitRate, real stepDuration,vector rewardDelays, real sigma) {
  vector[num_elements(rewardDelays)] action;
  vector[num_elements(rewardDelays)] output;
  real endStep;
  real rewardDelay;
  int t;

  for(n in 1 : num_elements(rewardDelays)){
    rewardDelay  = rewardDelays[n];
    endStep = round(rewardDelay / stepDuration);
    action[n] = 10;
    t = 0;
    while(t <= endStep){
      t = t + 1;
      if(uniform_rng(0, 1) > waitRate ){
        action[n] = 0;
        break;
      }
    }
    output[n] <- normal_rng(action[n], sigma);
  }
  return output;
  }
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
fixed_ratio_wait_model <- "
data {
  int N;
  real sigma;
  real stepDuration;
  vector[N] rewardDelays;
  vector[N] actions;
}
parameters {
  real<lower = 0, upper = 1> waitRate;

}
model {
  waitRate ~ uniform(0, 1); 
  // The likelihood
  vector[N] predictions;
  int t = 0;
  for(n in 1 : N){
    real rewardDelay  = rewardDelays[n];
    real endStep = round(rewardDelay / stepDuration);
    predictions[n] = 10;
    while(t <= endStep){
    t = t + 1;
    if(uniform_rng(0, 1) > waitRate ){
    predictions[n] = 0;
    break;
    }
  }
output[n] <- normal_rng(action[n], sigma);
}
  actions ~ normal(outputs, sigma);
}
generated quantities {
  // For model comparison, we'll want to keep the likelihood contribution of each point
  vector[N] log_lik;
  vector[N] y_sim;
  for(i in 1:N){
  log_lik[i] <- normal_log(y[i], nu, X[i,]*beta, sigma);
  y_sim[i] <- normal_rng(nu, X[i,]*beta, sigma);
  }
}
"
data_list <- list(N = length(thisBlockData$scheduledWait),
                  rewardDelays = thisBlockData$scheduledWait,
                  actions = thisBlockData$trialEarnings,
                  sigma = 1,
                  stepDuration = 0.5)
fit <- stan(model_code = fixed_ratio_wait_model, data = data_list, cores = 1, chains = 2, iter = 2000)
