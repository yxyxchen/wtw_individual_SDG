######### load recover results and auc results #######
load('outputs/expData/groupData.RData')
load('outputs/expData/waitRecover.RData')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')

############ prepare dataset #######
junkData = data.frame(groupData[groupData$blockNum == 1, ], LLs, solutions)
colnames(junkData) = c(colnames(junkData)[1 : ncol(groupData)], 'LL', 'phi', 'tau', 'gamma')

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

ggplot(junkData,aes(LL, fill = condition)) + geom_histogram(bins = 30) + facet_grid(~condition) +
  scale_fill_manual(values = conditionColors) + saveTheme 
ggsave('outputs/exp_figures/LL.pdf', width = 6, height = 3)
############ plot relationship between learning paras and auc ###########


# correlation between AUC and parameters
library('ggplot2')

for(cIdx in 1 : 2){
  cond = conditions[cIdx]
  fitData = junkData[junkData$condition == cond,]
  corPhi = cor.test(fitData$AUC, fitData$phi, method = 'pearson')
  corTau = cor.test(fitData$AUC, fitData$tau, method = 'pearson')
  corGamma = cor.test(fitData$AUC, fitData$gamma, method = 'pearson')
  
  plotData = data.frame(auc = fitData$AUC, phi = fitData$phi,
                        tau = fitData$tau, gamma = fitData$gamma)
  
  ggplot(plotData, aes(auc, phi)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    annotate('text',  x = 15, y = 0.3,label = sprintf('%.2f(p = %.2f)', corPhi$estimate, corPhi$p.value),
             size = 6, color = 'blue')
  filename = sprintf('outputs/exp_figures/phi_auc_%s.pdf', cond)
  ggsave(filename, width = 6, height = 5)
  
  ggplot(plotData, aes(auc, tau)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    annotate('text',  x = 15, y = 0.3,label = sprintf('%.2f(p = %.2f)', corTau$estimate, corTau$p.value),
             size = 6, color = 'blue')
  filename = sprintf('outputs/exp_figures/tau_auc_%s.pdf', cond)
  ggsave(filename, width = 6, height = 5)
  
  ggplot(plotData, aes(auc, gamma)) + geom_point() + saveTheme + ggtitle(cond) + geom_smooth(method = 'lm') +
    annotate('text',  x = 15, y = 0.3,label = sprintf('%.2f(p = %.2f)', corGamma$estimate, corGamma$p.value),
             size = 6, color = 'blue')
  filename = sprintf('outputs/exp_figures/gamma_auc_%s.pdf', cond)
  ggsave(filename, width = 6, height = 5)
}

#### relationship between parameters and stress
t.test(fitData$phi[fitHPData$stress == 1],
       fitData$phi[fitHPData$stress == 2])
t.test(fitData$tau[fitHPData$stress == 1],
       fitData$tau[fitHPData$stress == 2])
t.test(fitData$gamma[fitHPData$stress == 1],
       fitData$gamma[fitHPData$stress == 2])



