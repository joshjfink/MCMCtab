data {
  int<lower=0> N;
  int<lower=0> n_age;
  int<lower=0> n_cntry;

  int<lower=1,upper=n_age> age[N];

  vector<lower=0,upper=1>[N] female;

  int<lower=1,upper=n_cntry> cntry[N];

  vector[n_cntry] foreignpct;

  int<lower=0,upper=1> y[N];
} 
parameters {
  vector[n_age] b_age;

  vector[n_cntry] b_cntry;

  real b_foreignpct;

  vector[4] beta;

  real mu;
  real mu_age;

  real<lower=0,upper=100> sigma_age;
  real<lower=0,upper=100> sigma_cntry;

  real b_0;
  real b_female;
  real b_unemp;
}
transformed parameters {
  vector[n_age] b_age_adj;
  vector[n_cntry] b_cntry_hat;
  real mu_adj;
  vector[N] Xbeta;
  vector[N] p;
  vector[N] p_bound;

  for (i in 1:N)
    Xbeta[i] <- beta[1] + beta[2]*female[i] +
      b_age[age[i]] + 
      b_cntry[cntry[i]];

  mu_adj <- beta[1] + mean(b_age) + 
  mean(b_cntry);

  b_age_adj <- b_age - mean(b_age);

# Currently no intercept here
  for (j in 1:n_cntry)
    b_cntry_hat[j] <-  100 * b_foreignpct * foreignpct[j]; 
}
model {
  mu_age ~ normal(0, 1);

  mu ~ normal(0, 100);

  sigma_age ~ uniform(0, 100);

  sigma_cntry ~ uniform(0, 100);

  beta ~ normal(0, 100);
  b_age ~ normal(100 * mu_age, sigma_age);

  b_cntry ~ normal(b_cntry_hat, sigma_cntry);

  b_foreignpct ~ normal(0, 1);

  y ~ bernoulli_logit(Xbeta);
}