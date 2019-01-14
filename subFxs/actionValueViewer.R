# unupdated vaWaits were not recorded, so here we manually
# set them identical with the last updated value
actionValueViewer = function(vaWaits, vaQuits, blockData, para){
  
  gamma = para[3];
  nTimeStep = nrow(vaQuits);
  
  # if vaWaits is not updated, then just take the value of the last trial
  for(i in 2 : endTick){
    if(sum(is.na(vaWaits[,i])) > 0){
      vaWaits[is.na(vaWaits[,i]),i]  = vaWaits[is.na(vaWaits[,i]),i-1] 
    }
  }
  
  # if vaQuit is not recorded, it is equal the next recording 
  for(i in 1 : endTick){
    if(sum(is.na(vaQuits[,i])) > 0){
      vaQuits[is.na(vaQuits[,i]),i] = vaQuits[match(NA,vaQuits[,i] ) - 1,i]
    }
  }
  
  # 
  endTick =  length(blockData$trialEarnings)
  nTimeStep = dim(vaQuits)[1]
  stepDuration = 0.5
  
  for(i in 1: endTick){
    cIdx = i
    plotData = data.frame(va =c(vaWaits[,cIdx], vaQuits[,cIdx]),
                          time = rep( 1 : nTimeStep, 2),
                          action = rep(c('wait', 'quit'),
                                       each = nTimeStep))
    trialTitle =  sprintf('Trial %d', i)
    preRewardTitle = sprintf(', preR = %d, preT = %.2f',
                             blockData$trialEarnings[i-1], blockData$waitDuration[i-1])
    nowRewardTitle = sprintf(', nowR = %d, nowT =%.2f',
                             blockData$trialEarnings[i], blockData$waitDuration[i])  
    label = paste(trialTitle, preRewardTitle, nowRewardTitle, sep = '')
    
    
    # waitDuration is not the real waiting time, since sometimes schedualedWait is used
    endStep = round(blockData$waitDuration[i] / stepDuration)
    p = ggplot(plotData, aes(time, va, color = action)) + geom_line() +
      geom_vline(xintercept = endStep) + xlim(c(-1, nTimeStep)) +
      ggtitle(label) + xlab('time step') + ylab('action value') + displayTheme
    print(p)
    readline(prompt = paste(i, '(hit ENTER for action valus in the next trial)'))
  }  
}