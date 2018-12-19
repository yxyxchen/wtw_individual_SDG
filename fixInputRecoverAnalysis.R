library('ggplot2')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/waitRecover.RData')
load('outputs/fixInputSimData/colpData.RData')

# plot waitRecover
plotData = data.frame(c(LLs$HP, LLs$LP), rbind(solutions$HP, solutions$LP),
                      rbind(initialSpace, initialSpace),
                      conditions = rep(c('HP', 'LP'), each = nComb))
colnames(plotData) = c('LL', 'phiHat', 'tauHat', 'gammaHat',
                       'phi', 'tau', 'gamma', 'cond')
plotData$phi = factor(plotData$phi)
plotData$tau = factor(plotData$tau)
plotData$gamma = factor(plotData$gamma)
ggplot(plotData, aes(phi, phiHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/waitRecover_phi.pdf', width = 8, height = 3)

ggplot(plotData, aes(tau, tauHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/waitRecover_tau.pdf', width = 8, height = 3)

ggplot(plotData, aes(gamma, gammaHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/waitRecover_gamma.pdf',width = 8, height = 3)

# 
hist(LLs$HP - colpHPData$waitWithinDelta ^ 2) 
hist(LLs$LP - colpLPData$waitWithinDelta ^ 2) 


########################
library('ggplot2')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/colpData.RData')
load('outputs/fixInputSimData/dvRecover.RData')
plotData = data.frame(c(LLs$HP, LLs$LP), rbind(solutions$HP, solutions$LP),
                      rbind(initialSpace, initialSpace),
                      conditions = rep(c('HP', 'LP'), each = nComb))
colnames(plotData) = c('LL', 'phiHat', 'tauHat', 'gammaHat',
                       'phi', 'tau', 'gamma', 'cond')
plotData$phi = factor(plotData$phi)
plotData$tau = factor(plotData$tau)
plotData$gamma = factor(plotData$gamma)
ggplot(plotData, aes(phi, phiHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/dvRecover_phi.pdf', width = 8, height = 3)

ggplot(plotData, aes(tau, tauHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/dvRecover_tau.pdf', width = 8, height = 3)

ggplot(plotData, aes(gamma, gammaHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/dvRecover_gamma.pdf',width = 8, height = 3)


hist(LLs$HP - colpHPData$waitWithinDelta ^ 2) 
hist(LLs$LP - colpLPData$waitWithinDelta ^ 2) 
library('plyr')
count(solutions$HP[,1])

count(solutions$HP[,1])
