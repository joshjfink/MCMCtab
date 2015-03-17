### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/fedata.csv" , header = T)

# Build sample data (also recode vars for stan)
n= nrow(cdata)
sampdat <- dplyr::filter(cdata, rownames(cdata) %in% sample (rownames(cdata), n/15, replace=F)) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  mutate(region = as.numeric(as.character(factor(cntry, labels=seq(1,length(unique(cntry)),1))))) %>% 
  mutate(state = ifelse(yr2006==1, region+16, region))  %>% 
  mutate(y = dspendlaw) %>% 
  arrange(state)

# Foreign Percent Country-year summaries
foreignpct_l2 <- dplyr::summarise(group_by(sampdat, state), mean(foreignpct))[,2]

##### Number of binary predictors (+1)! (Manually code)
D = integer(3+ 1)

# Generate stan data 
# stock_mod1 <-  
# attach(dplyr::select(sampdat, y, female, ptemp, unemp, age, as.numeric(agesq)))

attach(sampdat)
stock_dat1 <-  list(y=y, female=female, ptemp=ptemp, unemp=unemp, age=age, agesq=as.numeric(agesq),
  n_age=as.integer(max(sampdat$age)), 
  n_state=as.integer(max(sampdat$state)), 
  n_region=as.integer(max(sampdat$region)), 
  N=as.integer(nrow(sampdat)), 
  foreignpct=as.numeric(unlist(foreignpct_l2)), 
  region=as.integer(c(unique(sampdat$region))),
  state= as.integer(unique(sampdat$state)), D=D)

str(stock_dat1)

# Left off here!



stock_dat1 <- list(dataList.1, D=D)
str(stock_dat1)

# Will uncomment when model is finished
  # Expansion_stock.sf1 <- stan(file='Expansion_stock.stan',
                                  # data=stock_dat1, iter=1000, chains=4)

## Subset Variables for models
  # imm_vars <- c("foreignpct", "migpct", "foreigndif", "dforeignpctnew") 
  # for (i in imm_vars){

### Code the Model
  # See STAN model Expansion_stock.stan

