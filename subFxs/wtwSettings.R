#source('plotTheme.R')

######## condition varibles #########
conditions = c("unif16", "log_1.75_32")
conditionNames = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")

######## timing variables ########
tMaxs = c(16, 32) # trial durations
blockMins = 15 # block duration in mins
blockSecs = blockMins * 60 # block duration in secs
iti = 2 # iti duration in secs
tGrid = seq(0, blockSecs, 0.1)

######### reward variable ########
tokenValue = 5 #value of the token
## reward timings in HP
HPTimings = seq(2, 16, by = 2) 
## reward timings in LP
n = 8
m = 32
fac = 1.75
d1 = log(m/(fac^n - 1))
d2 = log(m + m/(fac^n - 1))
tempt = exp(seq(d1,d2,length.out = n+1))
LPTimings = tempt[2:length(tempt)] - tempt[1]
timings = list(
  HP = HPTimings,
  LP = LPTimings
)
rm(m, fac, d1, d2, tempt, HPTimings, LPTimings)

########## supporting vairbales ########
# time ticks within a trial for timeEarnings or wtw analysis
trialTicks = list(
  'HP' = round(seq(0, tMaxs[1], by = 0.1), 1),
  'LP' = round(seq(0, tMaxs[2], by = 0.1), 1)
)

########## additional  variables for optimal analysis ########
# CDF of reward delays: p(t_reward <= T)
rewardDelayCDF = list(
 HP = approx(c(0, timings$HP), seq(0, 1, 1/n), xout = trialTicks$HP, method = "constant")$y, 
 LP = approx(c(0, round(timings$LP,1)), seq(0, 1, 1/n), xout = trialTicks$LP, method = "constant")$y
)

#  PDF of reward delays: p(t_reward = T)
HP = rep(0, length(trialTicks$HP))
HP[trialTicks$HP %in% timings$HP] = 1 / n 
LP = rep(0, length(trialTicks$LP))
# can't find ticks exactly equal to the timings 
# find cloest one 
LP[trialTicks$LP %in% round(timings$LP, 1)] = 1 / n 
rewardDelayPDF = list(
  "HP" = HP,
  "LP" = LP
)

# E(t_reward | t_reward <= T) 
HP = cumsum(trialTicks$HP * rewardDelayPDF$HP) / cumsum(rewardDelayPDF$HP)
LP = cumsum(trialTicks$LP * rewardDelayPDF$LP) / cumsum(rewardDelayPDF$LP)
# no reward arrives before the first reward timing, so points before that turn to NAN
meanRewardDelay = list('HP' = HP, 'LP' = LP)

# rewardRate
HP = tokenValue * rewardDelayCDF$HP /
  (meanRewardDelay$HP * rewardDelayCDF$HP + trialTicks$HP * (1 - rewardDelayCDF$HP) + iti)
LP = tokenValue * rewardDelayCDF$LP /
  (meanRewardDelay$LP * rewardDelayCDF$LP + trialTicks$LP * (1 - rewardDelayCDF$LP) + iti)
# quitting before the first reward timing get 0 reward
HP[which(is.nan(HP))] = 0 
LP[which(is.nan(LP))] = 0 
rewardRate = list('HP' = HP, 'LP' = LP)

optimWaitTimes = list()
optimWaitTimes$HP = trialTicks$HP[which.max(HP)]
optimWaitTimes$LP = trialTicks$LP[which.max(LP)]

optimRewardRates = list()
optimRewardRates$HP = max(HP)
optimRewardRates$LP = max(LP)
  
# # plot rewardRate
# for(c in 1:2){
#   thisCond = conditionNames[c];
#   plotData = data.frame(rewardRate = rewardRate[[thisCond]],
#                         waitThreshold = trialTicks[[thisCond]] )
#   opRewardRate = max(plotData$rewardRate);
#   opWaitThreshold = plotData$waitThreshold[which.max(plotData$rewardRate)]
#   opRewardRate * blockSecs
#   ggplot(plotData, aes(waitThreshold, rewardRate)) + geom_point(size = 1) +
#     geom_vline(xintercept = opWaitThreshold, size = 1, color = 'red', linetype  = 2) + 
#     xlab('Wait threshold / s') + ylab('Reward rate') + saveTheme  + ggtitle(thisCond)
#   fileName = sprintf("figures/rewardRate%s.pdf", thisCond)
#   ggsave(fileName, width = 6, height = 4)
# }



