######### load recover results and auc results #######
load('outputs/expData/groupData.RData')
load('outputs/expData/actionRecover.RData')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')

############ prepare dataset #######
junkData = data.frame(groupData, rep(negLLs, 3), rep(solutions[,1], each = 3), rep(solutions[,2], each = 3), rep(solutions[,3], each = 3))
colnames(junkData) = c("id", "blockNum", "cbal", "condition", "stress", "AUC", "totalEarnings",
                       'negLL', 'phi', 'tau', 'gamma')
junkData$blockNum = as.factor(junkData$blockNum)

############ info of fitting ########
ggplot(junkData,aes(phi, fill = condition)) + geom_histogram(bins = 30) + facet_grid(~condition) +
  scale_fill_manual(values = conditionColors) + saveTheme 
ggsave('outputs/exp_figures/phi.pdf', width = 6, height = 3)

ggplot(junkData,aes(tau, fill = condition)) + geom_histogram(bins = 30) + facet_grid(~condition) +
  scale_fill_manual(values = conditionColors) + saveTheme 
ggsave('outputs/exp_figures/tau.pdf', width = 6, height = 3)

ggplot(junkData,aes(gamma, fill = condition)) + geom_histogram(bins = 30) + facet_grid(~condition) +
  scale_fill_manual(values = conditionColors) + saveTheme 
ggsave('outputs/exp_figures/gamma.pdf', width = 6, height = 3)

ggplot(junkData,aes(negLL, fill = condition)) + geom_histogram(bins = 30) + facet_grid(~condition) +
  scale_fill_manual(values = conditionColors) + saveTheme 
ggsave('outputs/exp_figures/LL.pdf', width = 6, height = 3)
############ plot relationship between learning paras and auc ###########
# calculate correlation for each AUC 
phiLabel = vector(length = nrow(junkData))
tauLabel = vector(length = nrow(junkData))
gammaLabel = vector(length = nrow(junkData))
for(cIdx in 1 : 2){
  cond = conditions[cIdx]
  for(bIdx in 1 : 3){
    fitData = junkData[junkData$condition == cond & junkData$blockNum == bIdx,]
    corPhi = cor.test(fitData$phi, fitData$AUC, method = 'pearson')
    phiLabel[junkData$condition == cond & junkData$blockNum == bIdx] = sprintf('%.2f(p = %.2f)', corPhi$estimate, corPhi$p.value)
    
    corTau = cor.test(fitData$tau, fitData$AUC, method = 'pearson')
    tauLabel[junkData$condition == cond & junkData$blockNum == bIdx] = sprintf('%.2f(p = %.2f)', corTau$estimate, corTau$p.value)
    
    
    corGamma = cor.test(fitData$gamma, fitData$AUC, method = 'pearson')
    gammaLabel[junkData$condition == cond & junkData$blockNum == bIdx] = sprintf('%.2f(p = %.2f)', corGamma$estimate, corGamma$p.value)
    
  }
}
junkData$phiLabel = phiLabel
junkData$tauLabel = tauLabel
junkData$gammaLabel = gammaLabel

# correlation between AUC and parameters
library('ggplot2')
for(cIdx in 1 : 2){
  cond = conditions[cIdx]
  fitData = junkData[junkData$condition == cond,]
  plotData = data.frame(auc = fitData$AUC, phi = fitData$phi,
                        tau = fitData$tau, gamma = fitData$gamma, blockNum = fitData$blockNum,
                        phiLabel = fitData$phiLabel, tauLabel = fitData$tauLabel, gammaLabel = fitData$gammaLabel)
  
  ggplot(plotData, aes(auc, phi)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    geom_text(aes(label = phiLabel),x =  ifelse(cond == 'HP', 5, 10), y =  ifelse(cond == 'HP', 0.04, 0.1), size = 6, color = 'blue') + facet_grid(.~blockNum)
  filename = sprintf('outputs/exp_figures/phi_auc_%s.pdf', cond)
  ggsave(filename, width = 12, height = 4)
  
  ggplot(plotData, aes(auc,tau)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    geom_text(aes(label = tauLabel),x = 15, y = 15, size = 6, color = 'blue') + facet_grid(.~blockNum)
  filename = sprintf('outputs/exp_figures/tau_auc_%s.pdf', cond)
  ggsave(filename, width = 12, height = 4)
  

  ggplot(plotData, aes(auc, gamma)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    geom_text(aes(label = gammaLabel),x = ifelse(cond == 'HP', 10, 20), y = 0.3, size = 6, color = 'blue') + facet_grid(.~blockNum)
  filename = sprintf('outputs/exp_figures/gamma_auc_%s.pdf', cond)
  ggsave(filename, width = 12, height = 4)
}

#### relationship between parameters and stress
fitData = junkData[junkData$condition == 'LP',]
t.test(fitData$phi[fitData$stress == 1],
       fitData$phi[fitData$stress == 2])
t.test(fitData$tau[fitData$stress == 1],
       fitData$tau[fitData$stress == 2])
t.test(fitData$gamma[fitData$stress == 1],
       fitData$gamma[fitData$stress == 2])



