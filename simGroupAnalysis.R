# this script analysized the simulation data on the group level

############ load data and functions #########
# library
library("ggplot2")
library("dplyr")
library("tidyr")
library('scales')
source(file = './subFxs/plotThemes.R')
source(file = './subFxs/wtwSettings.R')

# load the para space of the simulation
load('outputs/QStarData/initialSpace.RData')

# load simulation data 
load('outputs/QStarData/colpData.RData')
load('outputs/QStarData/rawWTW.RData')
load('outputs/QStarData/hdrData.RData')

# define output file
outFile = 'outputs/QStar_figures'
dir.create(outFile)

####### plot distribution of totalEarnings
plotData = data.frame(totalEarnings = c(colpHPData$totalEarnings, colpLPData$totalEarnings),
                      condition = rep(c("HP", "LP"), each = nComb), phi = initialSpace[,1],
                      tau = initialSpace[,2], gamma = initialSpace[,3],
                      lambda = initialSpace[,4], wIni = initialSpace[,5]
)

ggplot(plotData, aes(totalEarnings)) + geom_histogram(bins = 15) +
  facet_wrap(~condition, nrow = 1) + xlab('Total earnings') + ylab("Num of simulations") + saveTheme + xlim(c(0, 600))
fileName = file.path(outFile, 'earningSml.pdf')
ggsave(fileName, width = 16, height = 8)

# calculate range
summarise(group_by(plotData, condition),
          minEarning = min(totalEarnings),
          maxEarning = max(totalEarnings))

############ summarise para effects on total earnings ###########
paraValues = seq(0.2, 0.8, 0.3) 
summaryData = data.frame(condition = rep(c("HP", "LP"), each = nValue, nPara),
                         paraNames = rep(paraNames, each = nValue * 2),
                         paraValues = rep(paraValues, nPara * 2))
summaryData$paraNames = factor(summaryData$paraNames, levels = paraNames)

# summarise mu and sd
mu = rep(NA, nrow(summaryData))
std = rep(NA, nrow(summaryData))
tempt = summarise(group_by(plotData, condition, phi), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[1:6] = tempt$mu; std[1:6] = tempt$std
tempt = summarise(group_by(plotData, condition, tau), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[7:12] = tempt$mu; std[7:12] = tempt$std
tempt = summarise(group_by(plotData, condition, gamma), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[13:18] = tempt$mu; std[13:18] = tempt$std
tempt = summarise(group_by(plotData, condition, lambda), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[19:24] = tempt$mu; std[19:24] = tempt$std
tempt = summarise(group_by(plotData, condition, wIni), mu = mean(totalEarnings), std = sd(totalEarnings))
mu[25:30] = tempt$mu; std[25:30] = tempt$std
summaryData$mu = mu
summaryData$std = std
summaryData$ymin = mu - std
summaryData$ymax = mu + std

# plot 
for(c in 1:2){
  cond = conditionNames[c]
  ggplot(summaryData[summaryData$condition == cond,], aes(factor(paraValues), mu)) +
    geom_bar(stat = "identity", width=0.5, fill = conditionColors[c]) + geom_errorbar(aes(ymin = ymin, ymax = ymax), width=.2)+
    facet_wrap(~paraNames, nrow = 1)+ saveTheme +
    xlab("Parameter value") + ylab("Total Earnings") + ggtitle(cond) 
  fileName = file.path(outFile, sprintf("paraEffect%s.pdf", cond))
  ggsave(fileName, width = 16, height = 8) 
}

######### plot AUC against totalEarnings #######
# prepare data
plotData = rbind(as.data.frame(colpHPData[c(1,3)]),
                 as.data.frame(colpLPData[c(1,3)]))
plotData$condition = rep(c('HP', 'LP'), each = length(colpHPData$totalEarnings))
plotData = plotData %>% arrange(totalEarnings) %>%group_by(condition) %>%
  mutate(earningRank = rank(totalEarnings, ties.method = "first"))

# plot for LP
ggplot(plotData[plotData$condition == 'LP',], aes(AUC, totalEarnings)) + geom_point(size = 1.5) +
  saveTheme + ylab('Total earnings') + xlim(c(0, tMaxs[2])) + ylim(c(0, 500))
fileName = file.path(outFile, "AUCLP_earningsLP.pdf") 
ggsave(fileName, width = 6, height = 4)

# plot for HP
ggplot(plotData[plotData$condition == 'HP',], aes(AUC, totalEarnings)) + geom_point(size = 1.5) +
  saveTheme + ylab('Total earnings') + xlim(c(0, tMaxs[1])) + ylim(c(0, 500))
fileName = file.path(outFile, "AUCHP_earningsHP.pdf") 
ggsave(fileName, width = 6, height = 4)


######## plot the timeseries of wtw #######
meanValues = c(apply(rawWTW$HP, MARGIN = 3, FUN = mean), 
               apply(rawWTW$LP, MARGIN = 3, FUN = mean))
stdValues = c(apply(rawWTW$HP, MARGIN = 3, FUN = sd), 
               apply(rawWTW$LP, MARGIN = 3, FUN = sd))
plotData = data.frame(meanValues, stdValues,
                      time = rep(tGrid, time = 2),
                      condition = rep(c('HP', 'LP'), each = length(tGrid)),
                      minValues = meanValues - stdValues / sqrt(dim(rawWTW$HP)[1]),
                      maxValues = meanValues + stdValues / sqrt(dim(rawWTW$HP)[1]))

ggplot(plotData, aes(time, meanValues, color = condition)) + 
  geom_ribbon(data = plotData[plotData$condition == 'HP',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") +
  geom_ribbon(data = plotData[plotData$condition == 'LP',], aes(ymin=minValues, ymax=maxValues),linetype=0, alpha = 0.1, color = "#bababa") + 
  geom_line(size = 1) + xlab('Time in block / s') + ylab('WTW / s') + saveTheme 
fileName = file.path(outFile, "wtwTimeSeries.pdf")
ggsave(fileName, width = 12, height = 8)

########### plot HPAUC against LPAUC ##############
plotData = data.frame(HPAUC = colpHPData$AUC, LPAUC = colpLPData$AUC)
ggplot(plotData, aes(HPAUC, LPAUC)) + geom_point(shape = 3 ) + geom_smooth(method = lm) +
  xlab('HP AUC/s') + ylab("LP AUC /s") + saveTheme
fileName = file.path(outFile, "HPAUC_LPAUC.pdf")
ggsave(fileName, width = 8, height = 8)


########### look at raw data ##############
linearData = as.data.frame(initialSpace);
colnames(linearData) = c('phi', 'tau', 'gamma', 'lambda', 'wIni')
linearData$AUC = colpHPData$AUC
linearData = lapply(linearData[, colnames(linearData)], scale)
fit = lm(AUC ~ phi + tau + wIni + gamma + lambda, data = linearData)
summary(fit)

