# this script contains helper analysis functions 

# check the distribution of scheduled delays
# ...as measured in number of key presses (for the instrumental version of the task)
scheduledDelays <- function(blockData,blockLabel) {
  cat(sprintf('Scheduled delays for %s\n',blockLabel))
  bkDelays = blockData$scheduledWait
  print(summary(bkDelays))
  # empirical cumulative distribution of scheduled delays
  fn <- ecdf(blockData$scheduledWait)
  plot(fn, main = sprintf('Scheduled delays: %s',blockLabel), xlab='Scheduled delay (s)',
       ylab='Cumulative proportion', xlim=c(0,30))
  # autocorrelation function
  # acfOutput <- acf(bkDelays, lag.max=20, main = sprintf('Scheduled delays: %s',blockLabel))
}


# plot trialwise responses in detail
trialPlots <- function(blockData,blockLabel) {
  # vectors to be plotted
  rwdIdx = blockData$trialEarnings != 0
  quitIdx = blockData$trialEarnings == 0
  rwdTrialNo = blockData$trialNum[rwdIdx]
  quitTrialNo = blockData$trialNum[quitIdx]
  rwdSchedDelay = blockData$scheduledWait[rwdIdx]
  quitSchedDelay = blockData$scheduledWait[quitIdx]
  waitDuration = blockData$timeWaited
  quitTime = waitDuration[quitIdx]
  # other parameters
  nTrials = nrow(blockData)
  # make the plot and add series
  plotData = data.frame(trialNum = c(rwdTrialNo, quitTrialNo, quitTrialNo),
                        trialDuration = c(rwdSchedDelay, quitTime, quitSchedDelay),
                        condition = rep(c('reward', 'quit', 'quitSchedule'), time = 
                                        c(length(rwdTrialNo), length(quitTrialNo),
                                          length(quitTrialNo))))
  plotData$condition = factor( plotData$condition, levels = c('reward', 'quit', 'quitSchedule'))
  p = ggplot(plotData, aes(trialNum, trialDuration, color = condition)) + geom_point() +
  geom_line(data = plotData[plotData$condition != 'quitSchedule',],
            aes(trialNum, trialDuration, color = condition)) +
    scale_color_manual(values = c('blue', 'red', 'black')) + 
    xlab('Trial num') + ylab('Trial duration / s') + ggtitle(label) + displayTheme
  print(p)
}


# calculate kaplan-meier and area under the curve
kmsc <- function(blockData,tMax,blockLabel='',makePlot=FALSE,grid=0) {
  library(survival)
  waitDuration = blockData$timeWaited
  quitIdx = (blockData$trialEarnings == 0)
  # for rewarded trials, base the duration on the reward delivery time (not the subsequent response)
  waitDuration[!quitIdx] <- blockData$scheduledWait[!quitIdx]
  # fit the survival function
  kmfit <- survfit(Surv(waitDuration, quitIdx, type='right') ~ 1, 
                 type='kaplan-meier', conf.type='none', start.time=0, se.fit=FALSE)
  # extract elements of the survival curve object (?survfit.object)
  kmT = kmfit$time
  kmF = kmfit$surv
  # add a point at zero
  kmT = c(0, kmT)
  kmF = c(1, kmF)
  # keep only points up through tMax
  keepIdx = kmT<=tMax
  kmT <- kmT[keepIdx]
  kmF <- kmF[keepIdx]
  # extend the last value to exactly tMax
  kmT <- c(kmT, tMax)
  kmF <- c(kmF, tail(kmF,1))
  # calculate auc
  auc <- sum(diff(kmT) * head(kmF,-1))
  # plot if requested
  if (makePlot) {
   plotData = data.frame(kmT = kmT, kmF = kmF)
   p = ggplot(plotData, aes(kmT, kmF)) + geom_line() + xlab('Delay (s)') +
      ylab('Survival rate') + ylim(c(0,1)) + xlim(c(0,tMax)) +
        ggtitle(sprintf('KMSC: %s (AUC = %1.1f)',blockLabel,auc)) + 
        displayTheme
   print(p)
  }
  # put the survival curve on a standard grid
  kmOnGrid = vector()
  for (gIdx in 1:length(grid)) {
    g = grid[gIdx]
    # use the last point where t is less than or equal to the current grid value
    kmOnGrid[gIdx] = kmF[max(which(kmT<=g))]
  }
  return(list(kmT=kmT, kmF=kmF, auc=auc, kmOnGrid=kmOnGrid))
}

# simple version of kmsc
kmscSimple <- function(waitDuration, quitIdx, tMax, grid) {
  library(survival)
  # fit the survival function
  kmfit <- survfit(Surv(waitDuration, quitIdx, type='right') ~ 1, 
                   type='kaplan-meier', conf.type='none', start.time=0, se.fit=FALSE)
  # extract elements of the survival curve object (?survfit.object)
  kmT = kmfit$time
  kmF = kmfit$surv
  # add a point at zero
  kmT = c(0, kmT)
  kmF = c(1, kmF)
  # keep only points up through tMax
  keepIdx = kmT<=tMax
  kmT <- kmT[keepIdx]
  kmF <- kmF[keepIdx]
  # extend the last value to exactly tMax
  kmT <- c(kmT, tMax)
  kmF <- c(kmF, tail(kmF,1))
  # calculate auc
  auc <- sum(diff(kmT) * head(kmF,-1))
  # put the survival curve on a standard grid
  kmOnGrid = vector()
  for (gIdx in 1:length(grid)) {
    g = grid[gIdx]
    # use the last point where t is less than or equal to the current grid value
    kmOnGrid[gIdx] = kmF[max(which(kmT<=g))]
  }
  return(list(kmT=kmT, kmF=kmF, auc=auc, kmOnGrid=kmOnGrid))
}
# willingness to wait time-series
wtwTS <- function(blockData, tGrid, wtwCeiling, blockLabel, plotWTW) {
  trialWTW = numeric(length = nrow(blockData)) # initialize the per-trial estimate of WTW
  quitIdx = blockData$trialEarnings == 0
  # use either the rewardTime (for reward trials) or time waited (for quit trials)
  #   (not using time waited for reward trials because this includes the post-reward RT)
  timeWaited = blockData$scheduledWait # use rewardtime make more sense but sometime nan
  timeWaited[quitIdx] = blockData$timeWaited[quitIdx]
  ### find the longest time waited up through the first quit trial
  #   (or, if there were no quit trials, the longest time waited at all)
  #   that will be the WTW estimate for all trials prior to the first quit
  firstQuit = which(quitIdx)[1]
  if (is.na(firstQuit)) {firstQuit = nrow(blockData)} # if no quit, set to the last trial
  currentWTW = max(timeWaited[1:firstQuit])
  thisTrialIdx = firstQuit - 1
  trialWTW[1:thisTrialIdx] = currentWTW
  ### iterate through the remaining trials, updating currentWTW
  while (thisTrialIdx < nrow(blockData)) {
    thisTrialIdx = thisTrialIdx + 1
    if (quitIdx[thisTrialIdx]) {currentWTW = timeWaited[thisTrialIdx]}
    else {currentWTW = max(currentWTW, timeWaited[thisTrialIdx])}
    trialWTW[thisTrialIdx] = currentWTW
  }
  ### impose a ceiling value, since trial durations exceeding some value may be infrequent
  trialWTW = pmin(trialWTW, wtwCeiling)
  ### convert from per-trial to per-second over the course of the block
  timeWTW = numeric(length = length(tGrid)) # initialize output
  binStartIdx = 1
  thisTrialIdx = 0
  while (thisTrialIdx < nrow(blockData)) {
    thisTrialIdx = thisTrialIdx + 1
    binEndTime = blockData$sellTime[thisTrialIdx]
    binEndIdx = max(which(tGrid < binEndTime)) # last grid point that falls within this trial
    timeWTW[binStartIdx:binEndIdx] = trialWTW[thisTrialIdx]
    binStartIdx = binEndIdx + 1
  }
  # extend the final value to the end of the vector
  timeWTW[binStartIdx:length(timeWTW)] = trialWTW[thisTrialIdx]

  ### for testing
  # for testing: plot trialWTW on top of an individual's trialwise plot
  if(plotWTW){
    # for testing: plot timeWTW
    p = ggplot(data.frame(tGrid, timeWTW), aes(tGrid, timeWTW)) + geom_line() +
      xlab("Time in block (s)") + ylab("WTW (s)") + ggtitle(sprintf('WTW : %s', blockLabel)) +
      displayTheme
    print(p)
  }
  return(timeWTW)
}

# willingness to wait time-series
wtwTSSimple <- function(timeWaited, quitIdx, tGrid, wtwCeiling) {
  trialWTW = numeric(length(timeWaited)) # initialize the per-trial estimate of WTW
  sellTime = cumsum(timeWaited);
  itiTime = rep(iti, length(timeWaited))
  itiTime = c(0, itiTime[2 : length(itiTime)])
  itiTime = cumsum(itiTime)
  sellTime = sellTime + itiTime
  ### find the longest time waited up through the first quit trial
  #   (or, if there were no quit trials, the longest time waited at all)
  #   that will be the WTW estimate for all trials prior to the first quit
  firstQuit = which(quitIdx)[1]
  if (is.na(firstQuit)) {firstQuit = length(timeWaited)} # if no quit, set to the last trial
  currentWTW = max(timeWaited[1:firstQuit])
  thisTrialIdx = firstQuit - 1
  trialWTW[1:thisTrialIdx] = currentWTW
  ### iterate through the remaining trials, updating currentWTW
  while (thisTrialIdx < length(timeWaited)) {
    thisTrialIdx = thisTrialIdx + 1
    if (quitIdx[thisTrialIdx]) {currentWTW = timeWaited[thisTrialIdx]}
    else {currentWTW = max(currentWTW, timeWaited[thisTrialIdx])}
    trialWTW[thisTrialIdx] = currentWTW
  }
  ### impose a ceiling value, since trial durations exceeding some value may be infrequent
  trialWTW = pmin(trialWTW, wtwCeiling)
  ### convert from per-trial to per-second over the course of the block
  timeWTW = numeric(length = length(tGrid)) # initialize output
  binEndIdx = 0
  thisTrialIdx = 0
  while (thisTrialIdx < length(timeWaited)) {
    thisTrialIdx = thisTrialIdx + 1
    binEndTime = sellTime[thisTrialIdx]
    # if bindEndTime = 0, that is quit in the first trial, skip it 
    if(binEndTime != 0){
      binStartIdx = binEndIdx + 1  
      binEndIdx = max(which(tGrid < binEndTime)) # last grid point that falls within this trial
      timeWTW[binStartIdx:binEndIdx] = trialWTW[thisTrialIdx]
    }
  }
  # extend the final value to the end of the vector
  timeWTW[binStartIdx:length(timeWTW)] = trialWTW[thisTrialIdx]
  return(timeWTW)
}

getTimeEarnings = function(blockData, tGrid, blockLabel, plotTimeEarnings){
  trialEarings = blockData$trialEarnings
  sellingTime = blockData$sellTime
  
  # adjust beginning end the end 
  sellingTime = c(0, sellingTime, blockSecs)
  trialEarings = c(0, trialEarings, trialEarings[length(trialEarings)])
  outputs = approx(sellingTime, cumsum(trialEarings),xout = tGrid,
                   method = "constant")
  
  # plot
  if(plotTimeEarnings){
    plotData = data.frame(tGrid = outputs$x, cumEarnings = outputs$y)
    p = ggplot(plotData,aes(tGrid, cumEarnings)) + geom_line() +
      xlab("Time in block (s)") + ylab("Cumulative earnings") +
      ggtitle(sprintf('timeEarnings: %s', blockLabel)) + displayTheme   
    print(p)
  }

  # return
  timeEarnings = outputs$y
  return(timeEarnings)
}

rewardRT <- function(blockData, blockLabel, makePlot) {
  # identify positive or negative outcome trials, get delay and RT
  rwdIdx = blockData$trialEarnings > 0
  rwdRT = blockData$timeWaited[rwdIdx] - as.numeric(blockData$rewardTime[rwdIdx])
  rwdScheduledDelay = blockData$scheduledWait[rwdIdx]
  lossIdx = blockData$trialEarnings < 0
  lossRT = blockData$timeWaited[lossIdx] - as.numeric(blockData$rewardTime[lossIdx])
  lossScheduledDelay = blockData$scheduledWait[lossIdx]
  # test & plot RT as a function of preceding delay
  if (sum(c(rwdIdx, lossIdx)) > 9) { # require at least 10 observations
    corResult <- cor.test(c(rwdScheduledDelay, lossScheduledDelay), c(rwdRT, lossRT), method='spearman')
    rhoValue = corResult$estimate
    plot(1, type='n', xlim=c(0,30), ylim=c(0,2), bty='n', frame.plot=FALSE, 
         xlab='Delay (s)', ylab='RT (s)', main=sprintf('%s: rho = %1.2f',blockLabel,rhoValue))
    lines(rwdScheduledDelay, rwdRT, col='blue', type='p', lwd=2, pch=16)
    lines(lossScheduledDelay, lossRT, col='red', type='p', lwd=2, pch=16)
  }
}


