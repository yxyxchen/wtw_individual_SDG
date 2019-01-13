library('tidyr')
library('dplyr')
library('ggplot2')
library('plyr')
load('outputs/Stan/fixInputFitting.RData')
load('outputs/fixInputSimData/initialSpace.RData')

## check traceplot
traceplot(fit, pars = c("phi", "tau", "gamma"))

## check bias 
known_parameters <- data_frame(variable = c("phi","tau", "gamma"),
                               real_value = initialSpace[1,])
tempt = rstan::extract(fit, permuted = F, pars = c("phi", "tau", "gamma")) %>% # extract data
  adply(2, function(x) x) %>%  # change arrays into 2-d dataframe 
  select(-chains) %>% # delete one columns 
  gather(variable, value)  %>%  # change to the long formant
  left_join(known_parameters, by = "variable") # lookup 

ggplot(tempt, aes(x = value)) + 
  geom_density(fill = "orange", alpha = 0.5) + # Make it pretty
  facet_wrap(~ variable, scales = "free") +
  geom_vline(aes(xintercept = real_value), colour = "red") +
  ggtitle("Actual parameters and estimates\ncorrectly specified model\n")



