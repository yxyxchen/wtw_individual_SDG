library('ggplot2')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/waitRecover.RData')
load('outputs/fixInputSimData/colpData.RData')

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


ggplot(plotData, aes(phi, LL)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 

mean(sqrt(LLs$HP))