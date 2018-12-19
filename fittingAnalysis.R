######### analysis 
load('outputs/expData/groupData.RData')
load('outputs/expData/waitRecover.RData')
source('subFxs/wtwSettings.R')

fitData = data.frame(groupData[groupData$blockNum == 1, ], LLs, solutions)
colnames(fitData) = c(colnames(fitData)[1 : ncol(groupData)], 'LL', 'phi', 'tau', 'gamma')

# correlation between AUC and parameters
cIdx = 1
cond = conditions[cIdx]
fitData = fitData[fitData$condition == cond,]

cor.test(fitData$AUC, fitData$phi)
cor.test(fitData$AUC, fitData$tau)
cor.test(fitData$AUC, fitData$gamma)

library('ggplot2')
plotData = data.frame(auc = fitHPData$AUC, phi = fitHPData$phi,
                      tau = fitHPData$tau, gamma = fitHPData$gamma) 
ggplot(plotData, aes(auc, phi)) + geom_point() 
ggplot(plotData, aes(auc, tau)) + geom_point()
ggplot(plotData, aes(auc, gamma)) + geom_point()


plotData = data.frame(auc = fitLPData$AUC, phi = fitLPData$phi,
                      tau = fitLPData$tau, gamma = fitLPData$gamma) 
ggplot(plotData, aes(auc, phi)) + geom_point() 
ggplot(plotData, aes(auc, tau)) + geom_point()
ggplot(plotData, aes(auc, gamma)) + geom_point()

#### relationship between parameters and stress
t.test(fitData$phi[fitHPData$stress == 1],
       fitData$phi[fitHPData$stress == 2])
t.test(fitData$tau[fitHPData$stress == 1],
       fitData$tau[fitHPData$stress == 2])
t.test(fitData$gamma[fitHPData$stress == 1],
       fitData$gamma[fitHPData$stress == 2])



