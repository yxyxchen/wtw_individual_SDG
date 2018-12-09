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

################ plot heat map of across and within delta #########

mean(colpLPData$waitWithinDelta) * nTrials
mean(colpLPData$waitAcrossDelta)


mean(colpLPData$dvAcrossDelta)