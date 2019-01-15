# set up 
singleFittingStan = function(combIdx, cond, wIni, timeWaited, trialEarning, fileName, pars){
  tMax = ifelse(cond == "HP", tMaxs[1], tMaxs[2])
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
  tempt = sampling(object = model, data = data_list, cores = nChain, chains = nChain,
               iter = nIter, 
               show_messages = F) %>%
    rstan::extract(permuted = F, pars = pars) %>%
    adply(2, function(x) x) %>%  # change arrays into 2-d dataframe 
    select(-chains) %>% cbind(data.frame(combIdx = rep(combIdx, nIter * nChain), condition = rep(cond, nIter * nChain)))
    write.csv(tempt, file = fileName,row.names=FALSE)

}






