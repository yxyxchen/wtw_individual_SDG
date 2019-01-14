library('tidyr')
library('dplyr')
library('ggplot2')
library('plyr')
source('subFxs/wtwSettings.R')
source('subFxs/plotThemes.R')
load('outputs/fixInputSimData/initialSpace.RData')

getMode = function(value) {
  junk = density(value, bw = "nrd0", adjust = 1,kernel = c("gaussian"), n = 512)
  junk$x[which.max(junk$y)]
}

# load para fitting 
samplePara = read.csv('stanOutPuts/fixInputSimData/HP_1.txt')
junk = lapply(1 : nComb, function(x, y) read.csv(sprintf('stanOutPuts/fixInputSimData/HP_%d.txt', x))) 
rawHPPara = ldply (junk, data.frame)
junk = lapply(1 : nComb, function(x, y) read.csv(sprintf('stanOutPuts/fixInputSimData/LP_%d.txt', x))) 
rawLPPara = ldply (junk, data.frame)
rawPara = rbind(rawHPPara, rawLPPara) %>%
  gather(variable, value, -condition, -combIdx)
rawPara$variable = factor(rawPara$variable, levels = c("phi", "tau", "gamma"))
# summarise rawPara
colnames(initialSpace) = factor(paraNames, levels = paraNames)
realValue =  rbind(initialSpace, initialSpace) %>%
  cbind(data.frame(combIdx = rep(1 : nrow(initialSpace), 2), condition = 
          rep(c("HP", "LP"), each = nrow(initialSpace)))) %>%
  gather(variable, realValue, -combIdx, -condition) %>% arrange(condition, combIdx)

tempt = rawPara %>% group_by(condition, combIdx, variable) %>%
  dplyr::summarise(mu = mean(value), median = median(value), mode = getMode(value), std = sd(value)) 
infoPara = cbind(tempt, realValue = realValue$realValue)

## check which representation is better
# no obvious different
# infoParaLong = infoPara %>% gather(method, value, mu:mode) %>%
#   mutate(error = abs(value - realValue))
# ggplot(infoParaLong[infoParaLong$condition == 'LP',], aes(method, error)) +
#   geom_boxplot(outlier.size = -1) + facet_grid(variable~., scales = "free")

## recover accuracy
dir.create('stanOutPuts/fixInputSim_figures')
ylimList = list(c(0, 0.25), c(1,30), c(0, 1))
for(cIdx in 1:2){
  cond = conditions[cIdx]
  for(vIdx in 1 : nPara){
    var = paraNames[vIdx]
    ggplot(infoPara[infoPara$condition == cond & infoPara$variable == var, ],
           aes(factor(realValue), mode)) +
      geom_boxplot(outlier.size = -1) + saveTheme + ylim(ylimList[[vIdx]])
    fileName = sprintf('stanOutPuts/fixInputSim_figures/%s_%s.pdf', cond, var)
    ggsave(fileName, width = 4, height = 4)
  }
}

## can we improve it anyway
## look at each case 
source("stan/stanCaseAnalysis.R")
cIdx = 1
cond = conditions[cIdx]
junkData = infoPara[infoPara$condition == cond,]
combIdxList = tail(rank(junkData[junkData$variable == "gamma", ]$std), round(nComb * 0.2))
for(i in 1: length(combIdxList)){
  combIdx = combIdxList[i]
  casePara = read.csv(sprintf('stanOutPuts/fixInputSimData/HP_%d.txt', combIdx))
  realValue <- data.frame(variable = c("phi","tau", "gamma"),
                          real_value = initialSpace[combIdx,])
  stanCaseAnalysis(casePara, realValue)
  readline("continue")
}





