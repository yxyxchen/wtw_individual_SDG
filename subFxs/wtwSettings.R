#source('plotTheme.R')

######## condition varibles #########
conditions = c("HP", "LP")
conditionColors = c("#7b3294", "#008837")

######## timing variables ########
tMaxs = c(20, 40) # trial durations
blockMins = 7 # block duration in mins
blockSecs = blockMins * 60 # block duration in secs
iti = 2 # iti duration in secs
tGrid = seq(0, blockSecs, 0.1)

######### reward variable ########
tokenValue = 10 #value of the token

########## supporting vairbales ########
# time ticks within a trial for timeEarnings or wtw analysis
trialTicks = list(
  'HP' = round(seq(0, tMaxs[1], by = 0.1), 1),
  'LP' = round(seq(0, tMaxs[2], by = 0.1), 1)
)

########## additional  variables for optimal analysis ########
# CDF of reward delays: p(t_reward <= T)
k = 4
mu = 0
sigma = 2
pareto = list()
pareto[['k']] = k
pareto[['mu']] = mu
pareto[['sigma']] = sigma
HP = 1 / trialTicks$HP[length(trialTicks$HP)]  * trialTicks$HP
LP = 1 - (1 + k * (trialTicks$LP - mu) / sigma) ^ (-1 / k)
LP[length(trialTicks$LP)] = 1 
rewardDelayCDF = list(
  HP = HP,
  LP = LP
)

# library(ggplot2)
# source('plotThemes.R')
# plotData = data.frame(time = c(trialTicks$HP, trialTicks$LP),
#                       cdf = c(rewardDelayCDF$HP, rewardDelayCDF$LP),
#                       condition = c(rep('HP', length(trialTicks$HP)),
#                                     rep('LP', length(trialTicks$LP))))
# ggplot(plotData, aes(time, cdf, linetype = condition)) + geom_line() + xlab('Elapsed time / s') +
#   ylab('CDF') + ggtitle('Timing conditions') + saveTheme
# 
# ggsave('../outputs/exp_figures/timing_conditions.png', width = 3, height = 2)


#  PDF of reward delays: p(t_reward = T)
#  make it discrete 
HP = c(0, diff(rewardDelayCDF$HP))
LP = c(0, diff(rewardDelayCDF$LP))
rewardDelayPDF = list(
  "HP" = HP,
  "LP" = LP
)

# E(t_reward | t_reward <= T) 
HP = cumsum(trialTicks$HP * rewardDelayPDF$HP) / cumsum(rewardDelayPDF$HP)
HP[1] = NaN
LP = cumsum(trialTicks$LP * rewardDelayPDF$LP) / cumsum(rewardDelayPDF$LP)
LP[1] = NaN
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
  

# library(ggplot2)
# source('plotThemes.R')
# plotData = data.frame(time = c(trialTicks$HP, trialTicks$LP),
#                       rewardRate = c(rewardRate$HP, rewardRate$LP),
#                       condition = c(rep('HP', length(trialTicks$HP)), 
#                                     rep('LP', length(trialTicks$LP))))
# ggplot(plotData, aes(time, rewardRate, linetype = condition)) + geom_line() + xlab('Giving-up time /s') +
#   ylab('Total Earnings / cent') + ggtitle('Expected payoff functions') + saveTheme
# ggsave('../outputs/exp_figures/expected_payoff.png', width = 3, height = 2)


#### calculate action value #######
# suppose the strategy is always wait
# using specific discount
# assume rewards all arrives at the right side 
# r = 0.5
# for(c in 1 : 2){
#   cond = conditions[c]
#   trialTick = trialTicks[[cond]]
#   nTicks = length(trialTick)
#   thisDelayPDF = rewardDelayPDF[[cond]]
#   # 
#   actionValueWaits = rep(0, nTicks)
#   for(i in 1 : nTicks){
#     actionValueWaits[i] = sum(tokenValue * exp(- (trialTick[i : nTicks] - trialTick[i]) * r)* thisDelayPDF[i : nTicks] / sum( thisDelayPDF[i : nTicks]))    
#   }
#   if(c == 1) HP = actionValueWaits else LP = actionValueWaits
# }


