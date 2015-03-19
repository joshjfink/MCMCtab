### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/06data.csv" , header = T)

# Build sample data (also recode vars for stan)
n=nrow(cdata)
sampdat <- dplyr::filter(cdata, rownames(cdata) %in% sample (rownames(cdata), n/10, replace=F)) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  mutate(cntry = as.numeric(as.character(factor(cntry, labels=seq(1,length(unique(cntry)),1))))) %>% 
  mutate(y = dspendlaw) %>% 
  arrange(cntry)

foreignpct_l2 <- foreignpct

# Generate list data for Stan
attach(sampdat)
stock_dat1 <-  list(y=y, female=female, ptemp=ptemp, unemp=unemp, age=age, 
  n_age=as.integer(max(sampdat$age)),
  n_cntry=as.integer(max(sampdat$cntry)), 
  n_foreignpct=max(foreignpct),
  N=as.integer(nrow(sampdat)), 
  foreignpct=as.numeric(unlist(foreignpct_l2)), 
  cntry= as.integer(sampdat$cntry))
# Check list data
str(stock_dat1)

# Run the model (Parallel)
# Compile and save the model
stock_m1_comp <- stan(file='Redundant_stock.stan', data = stock_dat1, chains = 0)

Expansion_stock.sf1 <- stan(file='Redundant_stock.stan', data=stock_dat1, iter=400, chains=2)

