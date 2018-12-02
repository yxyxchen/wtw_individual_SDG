load('outputs/QStarData/rawHPData.RData')
load('outputs/QStarData/rawLPData.RData')
library('R.matlab')
writeMat("outputs/QStarData/rawHPData.mat",
         wsHP = rawHPData$ws,
         timeWaitedHP = rawHPData$timeWaited,
         trialEarningsHP = rawHPData$trialEarnings,
         rewardDelaysHP = rawHPData$rewardDelays,
         vaQuitsHP = rawHPData$vaQuits,
         vaWaitsHP = rawHPData$vaWaits
         )

writeMat("outputs/QStarData/rawLPData.mat",
         wsLP = rawHPData$ws,
         timeWaitedLP = rawLPData$timeWaited,
         trialEarningsLP = rawLPData$trialEarnings,
         rewardDelaysLP = rawLPData$rewardDelays,
         vaQuitsLP = rawLPData$vaQuits,
         vaWaitsLP = rawLPData$vaWaits
)

writeMat("outputs/QStarData/rawHPData.mat",
         wsHP = rawHPData$ws,
         timeWaitedHP = rawHPData$timeWaited,
         trialEarningsHP = rawHPData$trialEarnings,
         rewardDelaysHP = rawHPData$rewardDelays,
         vaQuitsHP = rawHPData$vaQuits,
         vaWaitsHP = rawHPData$vaWaits
)

load('outputs/QStarData/initialSpace.RData')
writeMat("outputs/QStarData/initialSpace.mat",
         initialSpace = initialSpace
)