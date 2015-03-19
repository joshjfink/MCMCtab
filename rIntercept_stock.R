### Set Working Direct, "C"lear Environment, and Load Packages
  setwd("/Universe/GitHub/MCMCtab")
  source("check_packages.R")
  check_packages(c("rstan", "xtable", "foreign", "data.table", "parallel", "dplyr", "epicalc","gmodels", "foreign", "stringr", "knitr"))
  detachAllData( ); rm(list=ls())

### Load in 2006 data with 25 countries
  cdata <- read.csv( "Data/06data.csv" , header = T)

# Build sample data (also recode vars for stan)
n=nrow(cdata)
sampdat <- dplyr::filter(cdata, rownames(cdata) %in% sample (rownames(cdata), n/3, replace=F)) %>%
  dplyr::select(dspendlaw, cntryyr,cntry, foreignpct, age ,agesq,female,ptemp,unemp) %>%
  mutate(cntry = as.numeric(as.character(factor(cntry, labels=seq(1,length(unique(cntry)),1))))) %>% 
  mutate(y = dspendlaw) %>% 
  arrange(cntry)

foreignpct_l2 <- sampdat$foreignpct

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

# Compile and save the model
stock_m1_comp <- stan(file='rIntercept_stock.stan', data = stock_dat1, chains = 0)

# Run the model (Parallel)
CL = makeCluster(3, outfile = 'parallel.log')
clusterExport(cl = CL, c("stock_dat1", "stock_m1_comp")) 
RIntercept_stock.sf1 <- mclapply(1:2, mc.cores = 3, function(i)  stan(file='rIntercept_stock.stan', data=stock_dat1, chains=1, iter=3000,chain_id = i, seed=34, warmup=1000))
stopCluster(CL)
fit <- sflist2stanfit(RIntercept_stock.sf1)

print(fit,  pars = "b")

pdf("Plots/rIntercept_trace.pdf")
t <- rstan::traceplot(fit,  pars = c("b", "a"))
dev.off()