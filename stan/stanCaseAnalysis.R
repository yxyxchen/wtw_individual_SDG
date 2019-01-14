stanCaseAnalysis = function(casePara, realValue){
  tempt = casePara %>% # delete one columns 
    select(-c("condition", "combIdx")) %>%
    gather(variable, value)  %>%  # change to the long formant
    left_join(realValue, by = "variable") # lookup 
  
  ggplot(tempt, aes(x = value)) + 
    geom_density(fill = "orange", alpha = 0.5, n = 512) + # Make it pretty
    facet_wrap(~ variable, scales = "free") +
    geom_vline(aes(xintercept = real_value), colour = "red") +
    ggtitle("Actual parameters and estimates")
  
}

