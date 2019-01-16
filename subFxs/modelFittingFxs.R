# set up 
modelFitting = function(cond, wIni, timeWaited, trialEarning, fileName, pars, model){
  tMax = ifelse(cond == "HP", tMaxs[1], tMaxs[2])
  condIdx = ifelse(cond =="HP", 1, 2)
  nChain = 2
  nIter = 5000
  nTimeStep = tMax / stepDuration
  nTimePoints = round(ifelse(trialEarnings >0, ceiling(timeWaited / stepDuration), floor(timeWaited / stepDuration) + 1))
  data_list <- list(tMax = tMax,
                    wIni = wIni,
                    nTimeStep = nTimeStep,
                    N = length(timeWaited),
                    timeWaited = timeWaited,
                    trialEarnings = trialEarnings,
                    nTimePoints = nTimePoints)
  # init = list(list('phi' = 0.5, 'tau' = 15, 'gamma' = 0.5),
  #             list('phi' = 0.1, 'tau' = 5, 'gamma' = 0.1))
  fit = sampling(object = model, data = data_list, cores = nChain, chains = nChain,
               iter = nIter, 
               show_messages = F) 
  tempt = fit %>%
    rstan::extract(permuted = F, pars = c(pars, "LL_all")) %>%
    adply(2, function(x) x) %>%  # change arrays into 2-d dataframe 
    select(-chains) 
  write.csv(tempt, file = sprintf("%s.txt", fileName), row.names=FALSE)
  log_lik = extract_log_lik(fit)
  WAIC = waic(log_lik)
  save("WAIC", file = sprintf("%s.RData", fileName))
}






