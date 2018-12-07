# load scripts, library and
source('subFxs/wtwSettings.R') 
load('outputs/simData/initialSpace.RData')
load('outputs/fixInputSimData/colpData.RData')
load('outputs/fixInputSimData/fixInputs.RData')
library('dplyr')
library('tidyr')
library('ggplot2')
source('subFxs/plotThemes.R')
source('subFxs/helperFxs.R')

################## compare waitDurations across para
ggplot(colpHPData, aes(dvSucessRecover)) + geom_histogram()
ggplot(colpHPData, aes(waitSucessRecover)) + geom_histogram()


############## summarise withinDelta and acrossDelta ############

paraValues = 1:5
paraData = data.frame(condition = rep(c("HP", "LP"), each = nValue, nPara),
                      paraNames = rep(paraNames, each = nValue * 2),
                      paraValues = rep(paraValues, nPara * 2))
paraData$paraNames = factor(paraData$paraNames, levels = paraNames)



# summarise mu 


muByPhi = summarise_at(group_by(plotData, condition, phi), vars(withinDelta : acrossDelta), mean)
muByTau = summarise_at(group_by(plotData, condition, tau), vars(withinDelta : acrossDelta), mean)
muByGamma = summarise_at(group_by(plotData, condition, gamma), vars(withinDelta : acrossDelta), mean)

# summarise sd
stdByPhi = summarise_at(group_by(plotData, condition, phi), vars(withinDelta : acrossDelta), sd)
stdByTau = summarise_at(group_by(plotData, condition, tau), vars(withinDelta : acrossDelta), sd)
stdByGamma = summarise_at(group_by(plotData, condition, gamma), vars(withinDelta : acrossDelta), sd)

# 
mu = rbind(muByPhi, muByTau, muByGamma);
mu = mu[, 3:4]
std = rbind(stdByPhi, stdByTau, stdByGamma)
std = std[,3:4]
max= mu + std
min = mu -std


summaryWithinData = cbind(paraData, mu[,1], std[,1], max[,1], min[,1]);
summaryAcrossData = cbind(paraData, mu[,2], std[,2], max[,2], min[,2]);
colnames(summaryWithinData) = c(colnames(paraData), 'mu', 'std', 'max', 'min')
colnames(summaryAcrossData) = c(colnames(paraData), 'mu', 'std', 'max', 'min')

# plot withinDelta
for(c in 1:2){
  cond = conditions[c]
  ggplot(summaryWithinData[summaryWithinData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + geom_errorbar(aes(ymin = min, ymax = max), width=.2)+
    facet_wrap(~paraNames, nrow = 1)+ saveTheme +
    xlab("Parameter value") + ylab("withDelta / s") + ggtitle(cond) 
  fileName = sprintf('outputs/fixInputSim_figures/paraWithinDelta%s.pdf', cond)
  ggsave(file = fileName, width = 16, height = 8) 
}

# plot acrossDelta
for(c in 1:2){
  cond = conditions[c]
  ggplot(summaryAcrossData[summaryAcrossData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + geom_errorbar(aes(ymin = min, ymax = max), width=.2)+
    facet_wrap(~paraNames, nrow = 1)+ saveTheme +
    xlab("Parameter value") + ylab("acrossDelta / s") + ggtitle(cond) 
  fileName = sprintf('outputs/fixInputSim_figures/paraAcrossDelta%s.pdf', cond)
  ggsave(file = fileName, width = 16, height = 8) 
}


####### SUCESSS in predict 
withinDelateMuHP = regData$withinDelta[regData$condition == 'HP']
sucessHP = vector(length = nComb)
for(i in 1 : nComb){
  junk = acrossDelta$HP[, i]
  junk = junk[!is.na(junk)]
  sucessHP[i] = sum(withinDelateMuHP[i] < junk ) / (nComb - 1)
}
hist(sucessHP)
