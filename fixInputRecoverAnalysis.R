library('ggplot2')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/actionRecover.RData')
load('outputs/fixInputSimData/colpData.RData')

# plot actionRecover
plotData = data.frame(c(negLLs$HP, negLLs$LP), rbind(solutions$HP, solutions$LP),
                      rbind(initialSpace, initialSpace),
                      conditions = rep(c('HP', 'LP'), each = nComb))
colnames(plotData) = c('LL', 'phiHat', 'tauHat', 'gammaHat',
                       'phi', 'tau', 'gamma', 'cond')
plotData$phi = factor(plotData$phi)
plotData$tau = factor(plotData$tau)
plotData$gamma = factor(plotData$gamma)
ggplot(plotData, aes(phi, phiHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/actionRecover_phi.pdf', width = 8, height = 3)

ggplot(plotData, aes(tau, tauHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/actionRecover_tau.pdf', width = 8, height = 3)

ggplot(plotData, aes(gamma, gammaHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/actionRecover_gamma.pdf',width = 8, height = 3)

# 
hist(negLLs$HP - colpHPData$waitWithinDelta ^ 2) 
hist(negLLs$LP - colpLPData$waitWithinDelta ^ 2) 


