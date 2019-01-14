options(warn=-1, message =-1) # default settings borrowed somewhere
library(dplyr); library(ggplot2);library(reshape2); library('rstan'); #load libraries
library('plyr')
Sys.setenv(USE_CXX14=1) # making rstan working on this device 
rstan_options(auto_write = TRUE) # default settings borrowed somewhere
options(mc.cores = parallel::detectCores())# enable multi-core precessors 
library('tidyr')
source('stan/singleFittingStan.R')

model = stan_model(file = "stan/model.stan")
for(i in 7: 125){
  singleFittingStan(2, i)
}

model = stan_model(file = "stan/model.stan")
for(i in 1: 125){
  singleFittingStan(2, i)
}