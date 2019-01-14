library('tidyr')
library('dplyr')
library('ggplot2')
library('plyr')
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
# summarise 
colnames(initialSpace) = factor(paraNames, levels = paraNames)
realValue =  rbind(initialSpace, initialSpace) %>% cbind(data.frame(combIdx = 1 : nrow(initialSpace))) %>%
  gather(variable, realValue, -combIdx) %>% arrange(combIdx) 
tempt = rawPara %>% group_by(condition, combIdx, variable) %>%
  dplyr::summarise(mu = mean(value), median = median(value), mode = getMode(value)) 
  




## check traceplot
# traceplot(fit, pars = c("phi", "tau", "gamma"))

## check bias 
known_parameters <- data_frame(variable = c("phi","tau", "gamma"),
                               real_value = initialSpace[1,])
tempt = samplePara %>% # delete one columns 
  select(-c("condition", "combIdx")) %>%
  gather(variable, value)  %>%  # change to the long formant
  left_join(known_parameters, by = "variable") # lookup 

ggplot(tempt, aes(x = value)) + 
  geom_density(fill = "orange", alpha = 0.5, n = 512) + # Make it pretty
  facet_wrap(~ variable, scales = "free") +
  geom_vline(aes(xintercept = real_value), colour = "red") +
  ggtitle("Actual parameters and estimates\ncorrectly specified model\n")



v = samplePara$tau
uniqv <- unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
