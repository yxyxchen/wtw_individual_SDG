########### load scripts, library ################
# basic
library('dplyr')
library('tidyr')
library('ggplot2')
source('subFxs/plotThemes.R')
source('subFxs/wtwSettings.R')
load('outputs/fixInputSimData/rawLPData.RData')
load('outputs/fixInputSimData/colpData.RData')
load('outputs/simData/initialSpace.RData')

################## compare waitDurations across para
plotData = data.frame(condition = rep(c('HP', 'LP'), each = nComb), 
                      dvSucessRecover = c(colpHPData$dvSucessRecover, colpLPData$dvSucessRecover),
                      waitSucessRecover = c(colpHPData$waitSucessRecover, colpLPData$waitSucessRecover))
ggplot(plotData, aes(dvSucessRecover, fill = condition)) + geom_histogram(bins = 30) +facet_grid(.~condition) +
  scale_fill_manual(values = c(conditionColors))+
  xlab(expression("p(within"~Delta~"dv"~"<"~"across"~Delta~'dv)')) + saveTheme
ggsave(filename = 'outputs/fixInputSim_figures/dvSucessRecover.pdf', width = 8, height = 4)


plotData = data.frame(condition = rep(c('HP', 'LP'), each = nComb), 
                      dvSucessRecover = c(colpHPData$dvSucessRecover, colpLPData$dvSucessRecover),
                      waitSucessRecover = c(colpHPData$waitSucessRecover, colpLPData$waitSucessRecover))
ggplot(plotData, aes(dvSucessRecover, fill = condition)) + geom_histogram(bins = 30) +facet_grid(.~condition) +
  scale_fill_manual(values = c(conditionColors))+
  xlab(expression("p(within"~Delta~"dv"~"<"~"across"~Delta~'dv)')) + saveTheme
ggsave(filename = 'outputs/fixInputSim_figures/dvSucessRecover.pdf', width = 8, height = 4)

ggplot(plotData, aes(waitSucessRecover, fill = condition)) + geom_histogram(bins = 30) +facet_grid(.~condition) +
  scale_fill_manual(values = c(conditionColors))+
  xlab(expression("p(within"~Delta~"wait"~"<"~"across"~Delta~'wait)')) + saveTheme
ggsave(filename = 'outputs/fixInputSim_figures/waitSucessRecover.pdf', width = 8, height = 4)


########## 
plotData = data.frame(withinDelta = colpHPData$dvWithinDelta, phi = initialSpace[,1],
                      tau = initialSpace[,2], gamma = initialSpace[,3])
summarise(group_by(plotData, phi), mu = mean(withinDelta))

summarise(group_by(plotData, tau), mu = mean(withinDelta))

summarise(group_by(plotData, gamma), mu = mean(withinDelta))

################ plot heat map of across and within delta #########
load('outputs/fixInputSimData/pairCompareData.RData')
plotData = dvDelta[['HP']]
plotData = as.data.frame(plotData)
rg = 1:25
plotData = plotData[rg, rg]
plotData$phi = initialSpace[rg,1]
plotData$tau = initialSpace[rg,2]
plotData$gamma = initialSpace[rg,3]
plotData = data.frame(i = 1 : nrow(plotData), plotData)
colnames(plotData) = c("i", as.character(1 : nrow(plotData)), 'phi', 'tau', 'gamma')
plotData = gather(plotData, key = 'j', value = 'delta', -i, -phi, -tau, -gamma)
plotData = data.frame(plotData)
plotData$j = as.numeric(plotData$j)
plotData$samephi = as.numeric((plotData$i - plotData$j) %% 5 == 0)
plotData$sametau= (plotData$i %/% 5 == plotData$j %/% 5 )
plotData$samegamma = (plotData$i %/% 25 == plotData$j %/% 25 )
plotData$i = factor(plotData$i, levels = as.character(1 : 25))
plotData$j = factor(plotData$j, levels = as.character(1 : 25))
plotData$similarity = 1 / plotData$delta

  
  
ggplot(data = plotData, aes(i, j)) + geom_tile(aes(fill = similarity, color = samephi)) +
  scale_fill_gradient(low = "white", high = "red") + scale_color_gradient(low = "white", high = "black") +
  coord_fixed(ratio = 1) + scale_x_discrete(labels =  as.character(round(plotData$phi[1:25] * 10 ))) +
  scale_y_discrete(labels =  as.character(round(plotData$phi[1:25] * 10 )))


ggplot(data = plotData, aes(i, j)) + geom_tile(aes(fill = similarity)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed(ratio = 1) + scale_x_discrete(labels =  as.character(round(plotData$phi[1:25] * 10 ))) +
  scale_y_discrete(labels =  as.character(round(plotData$phi[1:25] * 10 )))

+ geom_abline(slope = 1, intercept = seq(-25, 25, by = 5))

ggplot(data = plotData, aes(i, j)) + geom_tile(aes(fill = ngDelta)) +
  scale_fill_gradient(low = "white", high = "steelblue") + geom_abline(slope = 1, intercept = 0) +
  coord_fixed(ratio = 1) + scale_x_discrete(labels =  as.character(round(plotData$tau[1:25]))) +
  scale_y_discrete(labels =  as.character(round(plotData$tau[1:25]  ))) 

ggplot(data = plotData, aes(i, j)) + geom_tile(aes(fill = ngDelta)) +
  scale_fill_gradient(low = "white", high = "steelblue") + geom_abline(slope = 1, intercept = 0) +
  coord_fixed(ratio = 1) + scale_x_discrete(labels =  as.character(round(plotData$gamma[1:25]))) +
  scale_y_discrete(labels =  as.character(round(plotData$gamma[1:25]  ))) 


######### example matrix data, support to explain above ##########
# nValue = 2
# nPara = 3
# nComb = nValue ^ nPara
# values = c(1,2)
# valueLabels = c('L', 'H')
# paraList = matrix(NA, nComb,nPara)
# paraList[,1] = rep(c(1, 2), nValue ^(nPara - 1))
# paraList[,2] = rep(c(1, 2), each = nValue, nValue ^(nPara - 2))
# paraList[,3] = rep(c(1, 2), each = nValue ^ (nPara - 1))
# 
# paraPairs = array(NA, dim = c(nComb, nComb, 3))
# for(i in 1 : nComb){
#   for(j in 1 : nComb){
#     paraPairs[i, j, ] = abs(paraList[i,] - paraList[j, ])
#   }
# }
# 
# for(p in 1 : nPara){
# deltaMatrix = data.frame(i = as.character(1 : nComb), paraPairs[,,p])
# colnames(deltaMatrix) = c('i', as.character(1 : nComb))
# deltaMatrix = gather(deltaMatrix, key = 'j', value = 'delta', -i )
# deltaMatrix$similarity = 1-deltaMatrix$delta
# 
# ggplot(data = deltaMatrix, aes(i, j)) + geom_tile(aes(fill = similarity ), color = 'grey') +
#   scale_fill_gradient(low = "white", high = "steelblue") +
#   coord_fixed(ratio = 1) + scale_x_discrete(labels = valueLabels[paraList[,p]]) +
#   scale_y_discrete(labels =  valueLabels[paraList[,p]] ) + xlab(paraNames[p]) + ylab(paraNames[p]) + saveTheme
# fileName = sprintf('outputs/fixInputSim_figures/exp_%s.pdf', paraNames[p])
# ggsave(fileName, width = 4, height = 4)
# }
# 
# for(p1 in 1 : (nPara - 1)){
#   for(p2 in (p1 + 1) : nPara){
#     deltaMatrix = data.frame(i = as.character(1 : nComb), paraPairs[,,p1] + paraPairs[,,p2])
#     colnames(deltaMatrix) = c('i', as.character(1 : nComb))
#     deltaMatrix = gather(deltaMatrix, key = 'j', value = 'delta', -i )
#     deltaMatrix$similarity = 1-deltaMatrix$delta
#     
#     ggplot(data = deltaMatrix, aes(i, j)) + geom_tile(aes(fill = similarity ), color = 'grey') +
#       scale_fill_gradient(low = "white", high = "steelblue") +
#       coord_fixed(ratio = 1) + scale_x_discrete(labels = valueLabels[paraList[,p1]]) +
#       scale_y_discrete(labels =  valueLabels[paraList[,p2]] ) + xlab(paraNames[p1]) + ylab(paraNames[p2]) + saveTheme
#     fileName = sprintf('outputs/fixInputSim_figures/cross_%s_%s.pdf', paraNames[p1], paraNames[p2])
#     ggsave(fileName, width = 4, height = 4)
#   }
# }


################ plot heat map of across and within delta #########
load('outputs/fixInputSimData/pairCompareData.RData')
for(cIdx in 1 : 2){
  cond = conditions[cIdx]
  plotData = dvDelta[[cond]]
  # add parameter values
  plotData = as.data.frame(plotData)
  plotData$phi = initialSpace[,1]
  plotData$tau = initialSpace[,2]
  plotData$gamma = initialSpace[,3]
  # add comb no
  plotData = data.frame(i = 1 : nrow(plotData), plotData)
  colnames(plotData) = c("i", as.character(1 : nrow(plotData)), 'phi', 'tau', 'gamma')
  # gather
  plotData = gather(plotData, key = 'j', value = 'delta', -i, -phi, -tau, -gamma)
  plotData = data.frame(plotData)
  # add same phi, same tau, same gamma
  plotData$j = as.numeric(plotData$j)
  plotData$samephi = as.numeric((plotData$i - plotData$j) %% 5 == 0)
  plotData$sametau= (plotData$i %/% 5 == plotData$j %/% 5 )
  plotData$samegamma = (plotData$i %/% 25 == plotData$j %/% 25 )
  
  plotData$similarity = 1 / plotData$delta
  
  
  ggplot(data = plotData, aes(i, j)) + geom_tile(aes(fill = delta)) +
    scale_fill_gradient(low = "white", high = "red", name = expression(Delta~"dv")) +coord_fixed(ratio = 1) + xlab('Comb No.')+  ylab('Comb No.')+
    saveTheme + ggtitle(cond) +  geom_abline(slope = 1, intercept = 0)
  fileName = sprintf('outputs/fixInputSim_figures/deltaDv_%s.pdf', cond)
  ggsave(fileName, width = 6, height = 6) 
  
}


