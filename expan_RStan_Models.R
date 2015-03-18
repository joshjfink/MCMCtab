### Set Working Direct, Clear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/fedata.csv" , header = T)

# Build sample data (also recode vars for stan)
n=nrow(cdata)
sampdat <- dplyr::filter(cdata, rownames(cdata) %in% sample (rownames(cdata), n/10, replace=F)) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, yr2006, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  mutate(region = as.numeric(as.character(factor(cntry, labels=seq(1,length(unique(cntry)),1))))) %>% 
  mutate(state = ifelse(yr2006==1, region+16, region))  %>% 
  mutate(y = dspendlaw) %>% 
  arrange(state)

# Foreign Percent Country-year summaries
foreignpct_l2 <- dplyr::summarise(group_by(sampdat, state), mean(foreignpct))[,2]

##### Number of binary predictors (+1)! (Manually code)
D = 3+1

# Generate list data for Stan
attach(sampdat)
stock_dat1 <-  list(y=y, female=female, ptemp=ptemp, unemp=unemp, age=age, 
  # agesq=as.integer(agesq), 
  n_age=as.integer(max(sampdat$age)),
  # n_agesq=as.integer(max(sampdat$agesq)), 
  n_state=as.integer(max(sampdat$state)), 
  n_region=as.integer(max(sampdat$region)), 
  N=as.integer(nrow(sampdat)), 
  foreignpct=as.numeric(unlist(foreignpct_l2)), 
  region=as.integer(c(unique(sampdat$region), unique(sampdat$region))),
  state= as.integer(sampdat$state), D=D)
# Check list data
str(stock_dat1)

# Variables in model(For building stan model code)
# Vars included in the model: 
# outcome: y 
# binary: female ptemp unemp
# integer: age region state
# contiuous/numeric: agesq foreignpct
# constant: N n_age n_agesq n_state n_region D

# Run the model (Parallel)
# Compile and save the model
stock_m1_comp <- stan(file='Expansion_stock.stan', data = stock_dat1, chains = 0)

CL = makeCluster(3, outfile = 'parallel.log')
clusterExport(cl = CL, c("stock_dat1", "stock_m1_comp")) 
sflist <- mclapply(1:2, mc.cores = 3, function(i)  stan(fit = stock_m1_comp, data=stock_dat1, chains=1, iter=1000,chain_id = i, seed=34))

fit <- sflist2stanfit(sflist)

# Expansion_stock.sf1 <- stan(file='Expansion_stock.stan', data=stock_dat1, iter=400, chains=2)





