### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/fedata.csv" , header = T)

### Subset Variables for models
  # imm_vars <- c("foreignpct", "migpct", "foreigndif", "dforeignpctnew")
# "police", "homicide", i,
  # for (i in imm_vars){

# Build sample data (also recode vars for stan)
n= nrow(cdata)
sampdat <- dplyr::filter(cdata, rownames(cdata) %in% sample (rownames(cdata), n/15, replace=F)) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  mutate(region = as.numeric(as.character(factor(cntry, labels=seq(1,length(unique(cntry)),1))))) %>% 
  mutate(state = ifelse(yr2006==1, region+16, region))  %>% 
  mutate(y = dspendlaw)

# Generate stan data 
dataList.2 <- list(dplyr::select(sampdat, y, region, state, female, ptemp, unemp, foreignpct)
  , n_age=unique(sampdat$age), n_state=unique(sampdat$state), n_region=unique(sampdat$region), N=nrow(sampdat))


# Break here (use end of R_Stan_Models.R to run in parallel)

