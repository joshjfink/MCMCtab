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
dataList.2 <- list(dplyr::select(sampdat, y, region, state, female, ptemp, unemp, foreignpct, age, agesq) , n_age=max(sampdat$age), n_state=max(sampdat$state), n_region=max(sampdat$region), N=nrow(sampdat))


### Code the Model
  # mod_code <- '
data {
  int<lower=0> N;  
  int<lower=0> n_age;
  int<lower=0> n_state;
  int<lower=0> n_region;
  int<lower=1,upper=n_age> age[N]; # Categorical-ish vars
  vector<lower=0,upper=1>[N]female; # Binary predictors
  vector<lower=0,upper=1>[N]ptemp;
  vector<lower=0,upper=1>[N]unemp;

# Break here 


  int<lower=1,upper=n_region> region[n_state];
  int<lower=1,upper=n_state> state[N];
  vector[n_state] v_prev;
  int<lower=0,upper=1> y[N];
} 
parameters {
  real b_0;
  real b_black;
  real b_female;
  real b_female_black;
  real b_v_prev_raw;
  vector[4] beta;
  vector[n_age_edu] b_age_edu;
  vector[n_age] b_age_raw;
  vector[n_edu] b_edu_raw;
  vector[n_region] b_region_raw;
  vector[n_state] b_state_raw;
  real mu;
  real mu_age_edu;
  real<lower=0> sigma_age_raw;
  real<lower=0> sigma_edu_raw;
  real<lower=0> sigma_region_raw;
  real<lower=0> sigma_state_raw;
  real<lower=0> sigma_age_edu_raw;
  real<lower=0> xi_age;
  real<lower=0> xi_edu;
  real<lower=0> xi_age_edu;
  real<lower=0> xi_state;
} 
transformed parameters {
  vector[N] Xbeta;
  vector[n_age] b_age;
  vector[n_age_edu] b_age_edu_adj;
  vector[n_edu] b_edu;
  vector[n_region] b_region;
  vector[n_state] b_state;
  vector[n_state] b_state_hat;
  real mu_adj;
  real<lower=0> sigma_age;
  real<lower=0> sigma_edu;
  real<lower=0> sigma_age_edu;
  real<lower=0> sigma_state;
  real<lower=0> sigma_region;

  b_age <- xi_age * (b_age_raw - mean(b_age_raw));
  b_edu <- xi_edu * (b_edu_raw - mean(b_edu_raw));
  b_age_edu_adj <- b_age_edu - mean(b_age_edu);
  b_region <- xi_state * b_region_raw;
  b_state <- xi_state * (b_state_raw - mean(b_state_raw));
  mu_adj <- beta[1] + mean(b_age) + mean(b_edu) + mean(b_age_edu) +
     mean(b_state);

  sigma_age <- xi_age*sigma_age_raw;
  sigma_edu <- xi_edu*sigma_edu_raw;
  sigma_age_edu <- xi_age_edu*sigma_age_edu_raw;
  sigma_state <- xi_state*sigma_state_raw;
  sigma_region <- xi_state*sigma_region_raw;     # not "xi_region"

  for (i in 1:N)
    Xbeta[i] <- beta[1] + beta[2]*female[i] + beta[3]*black[i] +
      beta[4]*female[i]*black[i] +
      b_age[age[i]] + b_edu[edu[i]] + b_age_edu[age_edu[i]] +
      b_state[state[i]];
  for (j in 1:n_state)
    b_state_hat[j] <- b_region_raw[region[j]] + b_v_prev_raw*v_prev[j];
}
model {
  mu ~ normal (0, 100);
  mu_age_edu ~ normal(0, 1);

  b_age_raw ~ normal(0, sigma_age_raw);
  b_edu_raw ~ normal(0, sigma_edu_raw);
  b_age_edu ~ normal(100 * mu_age_edu,sigma_age_edu);
  b_state_raw ~ normal(b_state_hat, sigma_state_raw);
  beta ~ normal(0, 100);

  b_v_prev_raw ~ normal(0, 100);
  b_region_raw ~ normal(0, sigma_region_raw);

  y ~ bernoulli_logit(Xbeta);
}
