data {
  int<lower=0> N;  
  int<lower=0> D;  
  int<lower=0> n_age;
  # int<lower=0> n_agesq;
  int<lower=0> n_state;
  int<lower=0> n_region;
  int<lower=1,upper=n_age> age[N]; # Categorical-ish vars
  # int<lower=1,upper=n_agesq> agesq[N];
  vector<lower=0,upper=1>[N]female; # Binary predictors
  vector<lower=0,upper=1>[N]ptemp;
  vector<lower=0,upper=1>[N]unemp;
  int<lower=1,upper=n_region> region[n_state];
  int<lower=1,upper=n_state> state[N];
  vector[n_state] foreignpct;
  int<lower=0,upper=1> y[N];
} 

parameters {
  real b_0;
  real b_ptemp; 
  real b_unemp;
  real b_female;
  real b_foreignpct_raw;
  vector[D] beta;
  # vector[n_agesq] b_agesq;
  vector[n_age] b_age_raw;
  vector[n_region] b_region_raw;
  vector[n_state] b_state_raw;
  real mu;
  # real mu_agesq;
  real<lower=0> sigma_age_raw;
  # real<lower=0> sigma_agesq_raw;
  real<lower=0> sigma_region_raw;
  real<lower=0> sigma_state_raw;
  real<lower=0> xi_age;
  # real<lower=0> xi_agesq;
  real<lower=0> xi_state;
} 

transformed parameters {
  vector[N] Xbeta;
  vector[n_age] b_age;
  # vector[n_agesq] b_agesq_adj;
  vector[n_region] b_region;
  vector[n_state] b_state;
  vector[n_state] b_state_hat;
  real mu_adj;
  real<lower=0> sigma_age;
  # real<lower=0> sigma_agesq;
  real<lower=0> sigma_state;
  real<lower=0> sigma_region;
  b_age <- xi_age * (b_age_raw - mean(b_age_raw));
  # b_agesq_adj <- b_agesq - mean(b_agesq);
  b_region <- xi_state * b_region_raw;
  b_state <- xi_state * (b_state_raw - mean(b_state_raw));
  mu_adj <- beta[1] + mean(b_age) + mean(b_state);
  # + mean(b_agesq)

  sigma_age <- xi_age*sigma_age_raw;
  # sigma_agesq <- xi_agesq*sigma_agesq_raw;
  sigma_state <- xi_state*sigma_state_raw;
  sigma_region <- xi_state*sigma_region_raw;     # not "xi_region"
  for (i in 1:N)
    Xbeta[i] <- beta[1] + beta[2]*female[i] + beta[3]*ptemp[i] +
      beta[4]*unemp[i] +
      b_age[age[i]]   +
      b_state[state[i]];
      # + b_agesq[agesq[i]]
  for (j in 1:n_state)
    b_state_hat[j] <- b_region_raw[region[j]] + b_foreignpct_raw*foreignpct[j];
}
model {
  mu ~ normal (0, 100);
  # mu_agesq ~ normal(0, 1);

  b_age_raw ~ normal(0, sigma_age_raw);
  # b_agesq ~ normal(100 * mu_agesq,sigma_agesq);
  b_state_raw ~ normal(b_state_hat, sigma_state_raw);
  beta ~ normal(0, 100);

  b_foreignpct_raw ~ normal(0, 100);
  b_region_raw ~ normal(0, sigma_region_raw);

  y ~ bernoulli_logit(Xbeta);
}

