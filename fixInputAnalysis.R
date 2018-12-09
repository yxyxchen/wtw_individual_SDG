########### load scripts, library ################
# basic
library('dplyr')
library('tidyr')
library('ggplot2')
source('subFxs/plotThemes.R')
load('outputs/fixInputSimData/rawLPData.RData')
load('outputs/fixInputSimData/colpData.RData')


################## compare waitDurations across para
ggplot(colpHPData, aes(dvSucessRecover)) + geom_histogram()
ggplot(colpHPData, aes(waitSucessRecover)) + geom_histogram()

mean(colpLPData$waitWithinDelta) * nTrials
mean(colpLPData$waitAcrossDelta)

x
mean(colpLPData$dvAcrossDelta)