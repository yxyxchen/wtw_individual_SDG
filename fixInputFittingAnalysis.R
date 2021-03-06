library('ggplot2')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/fixInputSimData/initialSpace.RData')
load('outputs/fixInputSimData/paraFitting.RData')
load('outputs/fixInputSimData/colpData.RData')
dir.create('outputs/fixInputSim_figures')

# initialSpace = matrix(NA, nValue^nPara, nPara)
# initialSpace[,1] = rep(seq(0.1, 0.5, length.out = 5), nValue^(nPara - 1)) # phi
# initialSpace[,2] = rep(rep(seq(2, 22, length.out = 5), each = nValue), nValue^(nPara - 2)) # tau
# initialSpace[,3] = rep(seq(0.8, 0.98, length.out = 5), each = nValue^2)
# 
plotData = data.frame(negLL = c(negLLs$HP, negLLs$LP),
                      condition = rep(c('HP', 'LP'), each = length(negLLs$HP)))
ggplot(plotData, aes(negLL, fill = condition)) + geom_histogram(bins = 10) +
  facet_grid(.~condition) + scale_fill_manual(values = conditionColors) + saveTheme
ggsave('outputs/fixInputSim_figures/negLL.pdf', width = 6, height = 4)

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
ggsave('outputs/fixInputSim_figures/phi_phiHat.pdf', width = 8, height = 3)

ggplot(plotData, aes(tau, tauHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/tau_tauHat.pdf', width = 8, height = 3)

ggplot(plotData, aes(gamma, gammaHat)) + geom_boxplot() + facet_grid(.~cond) +
  geom_point() + saveTheme 
ggsave('outputs/fixInputSim_figures/gamma_gammaHat.pdf',width = 8, height = 3)


#################### parameter pair
ggplot(plotData, aes(phiHat, tauHat)) + geom_point() + saveTheme + facet_grid(~cond) +
  ylim(c(-0.1, 31)) + xlim(c(-0.1, 1.1))
ggsave('outputs/fixInputSim_figures/phiHat_tauHat.pdf',width = 8, height = 4) 

ggplot(plotData, aes(phiHat, gammaHat)) + geom_point()+saveTheme + facet_grid(~cond) +
  ylim(c(-0.1, 1.1)) + xlim(c(-0.1, 1.1))
ggsave('outputs/fixInputSim_figures/phiHat_gammaHat.pdf',width = 8, height = 4)

ggplot(plotData, aes(tauHat, gammaHat)) + geom_point()+saveTheme + facet_grid(~cond)+
  ylim(c(-0.1, 1.1)) + xlim(c(-0.1, 31))
ggsave('outputs/fixInputSim_figures/tauHat_gammaHat.pdf',width = 8, height = 4)
