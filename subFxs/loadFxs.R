
# loadData.R
# varying-magnitude WTW

# each subject has:
#  header file (*hdr.txt)
#  main data file (ending both in .mat and .txt)
#  empty *keytimes.txt file, a vestige of the effort key-pressing version. 


loadAllData = function() {
  ##### step 1: organize hdrData
  # load summary data
  dataDir = 'data'
  fileName = sprintf('%s/SDGdataset.csv', dataDir)
  summaryData= read.csv(fileName)
  # adjust
  summaryData$stress = ifelse( summaryData$Condition == 'stress', 'stress', 'no_stress')
  summaryData$Task..1...unif..2...gp. = ifelse(summaryData$Task..1...unif..2...gp. == 1, 'HP', 'LP')
  
  # exclude AUC and totalEarnings from hdrData
  hdrData = summaryData[,-(4:17)]
  colnames(hdrData) = c('ID', 'stress', 'condition', 'cbal', 'perceivedStress',
                        'traitAnxiety', 'Gender', 'BDI', 'posAffect1', 'posAffect2', 
                        'negAffect1', 'negAffect2', 'uncertainty', 'delay',
                        'impulsive', 'postUnpleasant')
  # count number of subjects 
  nSubjects = nrow(summaryData)
  nBlocks = 3

  ##### step 2: load trialData
  # define data column names
  colnames = c('blockNum', 'trialNum', 'trialStartTime', 'nKeyPresses', 'scheduledWait',
               'rewardTime', 'timeWaited', 'sellTime', 'trialEarnings','totalEarnings')
  # 'timeWaited' is from trial onset to keypress (includes RT)
  # 'sellTime' is the keypress time relative to block onset.
  
  # initialize
  trialData = list()

  # loop over individual subjects
  for (sIdx in 1:nSubjects) {
    thisID = hdrData$ID[sIdx]
    thisCbal = hdrData$Cbal[sIdx]
    thisCond = hdrData$condition[sIdx]
    thisStress = hdrData$stress[sIdx]
    # loop over blocks
    junk = vector(nBlocks, mode = 'list')
    for (bkIdx in 1 : nBlocks){
      thisFile = list.files(path=dataDir, pattern=(sprintf('wtw_stress_SDG%d_bk%d_1.txt',thisID, bkIdx)))
      if (length(thisFile) != 1) {
        cat('Could not identify a single data file for subject',thisID,' block', bkIdx, '\n')
        browser()
      }
      junk[[bkIdx]] = read.csv(file.path(dataDir,thisFile), header=FALSE, col.names=colnames)
      
      # adjust blockNum
      junk[[bkIdx]]$blockNum = junk[[bkIdx]]$blockNum * bkIdx
    }
    
    d = rbind(junk[[1]], junk[[2]], junk[[3]])
    # add info from hdrData
    d$condition = rep(thisCond, nrow(d))
    d$stress = rep(thisStress, nrow(d))
    
    
    # add to the list of all subjects' data
    trialData[[thisID]] = d
  } # end of loop over subjects
  
  # return the 2 data frames in a named list
  outputData = list(hdrData=hdrData, trialData=trialData)
  return(outputData)
  
} 