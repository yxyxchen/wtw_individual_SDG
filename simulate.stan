data = {
  // specific data
  int<lower=0> n; // number of trials
  real<lower = 0> rewardDelays[n]; // reward delays
  int<lower = 0> trialEarnings[n]; 
  real<lower = 0> Waitedtimes[n];
  
  // task setting
  real<lower = 0> tMax; // trial duration
  real<lower = 0> stepDuration; //step duration
  real <lower = 0> timeTicks; //timeTicks
  real <lower = 0> iti;
}

parameters {
  real<lower = 0, upper = 1> phi; // learning rate
  real<lower=0> tau; // temperature para
  real<lower = 0, upper = 1> gamma; // discouting rate
  real<lower = 0, upper = 1> lambda; // decay rate
  real<lower = 0> Wini; // initial value of W
  }
  

transformed parameters{
  int <lower =0> nTimeStep;
  
  matrix[nTimeStep, n] evQuits; // estimation of evQuits
  matrix[nTimeStep, n] evWaits; // estimation of evWaits 
  vector[nTimeStep] ws;
  vector[nTimeStep] es;
  int<lower = 1> xs;
  real <lower = 0> stepGap;
  
  // assign values
  nTimeStep = tMax / stepDuration; // stepDuration;
  xs = 1;
  stepDuration = 1;
  
  // not sure whether need to initialize 
  int<lower = 1> tIdx;
  int<lower = 1> t;
  real<lower = 0> rewardDelay
  real vaQuit;
  real vaWait;
  real waitRate;
  
  
  for(tIdx in 1 : n){
    rewardDelay = rewardDelays[tIdx]
    for(t in 1 : nTimeStep){
      // action value
      vaQuit = ws[1] * gamma^(iti / stepDuration) # didn't consider iTi, to stop getting things to complex
      vaWait = ws[xs];
      
      // determine action
      waitRate = exp(vaWait * tau) / sum(exp(vaWait * tau)  + exp(vaQuit * tau) )
      if(is.na(waitRate)){
        waitRate = 1
      }
      action = ifelse(runif(1) < waitRate, 'wait', 'quit')
      
      // next reward 
      // determine whether reward occurs in the step t
      rewardOccur = rewardDelay <= timeTicks[t + 1] && rewardDelay > timeTicks[t] 
      
      // if rewarded and wait, get the tokenValue; otherwise, 0
      getReward = (action == 'wait' && rewardOccur);
      nextReward = ifelse(getReward, tokenValue, 0) 
      
      // dertime next state
      // go to the terminate state if at the final step or quit or reward arrives
      trialGoOn= (action == 'wait' && !rewardOccur && t < nTimeStep)
      if(trialGoOn){
        nextXs = xs + 1
      }else{
        nextXs  = 1
      }
      
      // update eligilibity trace
      // here stepGap meatured between At and At-1
      junk = rep(0, nTimeStep)
      junk[xs] = 1
      es =  gamma^stepGap * lambda * es + junk * c(action == "wait")
      
      // update stepGap
      stepGap = ifelse(trialGoOn, 1, iti / stepDuration)
      
      // update action value of quit and wait
      // here stepGap meatured between At and At+1
      delta = nextReward + c(gamma^(stepGap) * max(ws[nextXs], vaQuit)) -
        ifelse(action == 'wait', vaWait, vaQuit)
      // anything wrong with the delta here?
      ws = ws + phi * delta * es
      
      // save history of vaWaits and vaQuits
      if(tIdx < nTrial) vaWaits[t, tIdx + 1] = ws[t];#  next trial vaWaits tracks updated vaWait, namely ws[t]

      vaQuits[t, tIdx] = vaQuit; # current trial vaQuits tracks current vaQuit
      
      // update xs 
      xs = nextXs
      
      // if break, update trialEarnings and timeWaited
      // return output
      if(!trialGoOn){
        trialEarnings[tIdx] = ifelse(nextReward == tokenValue, tokenValue, 0);
        # if quit, quit at t, if wait, wait until t+1
        timeWaited[tIdx] = ifelse(getReward,NA, ifelse(action == "quit", timeTicks[t], timeTicks[t+1]))
        break
      }
    }  # one trial end
  }
}

model {
  
}

  
  
  for(i in 1 : n){
    rewardDelay = rewardDelays[i];
    trialEarning = trialEarning[i];
    
    
  }
}