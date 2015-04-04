### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

# Load in Trail Data and Print Parameters 
load("/Universe/GitHub/PoliceImm_ModelsData/Data/MODsm06data.RData")

