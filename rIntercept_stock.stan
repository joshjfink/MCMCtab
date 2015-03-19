data {
  int<lower=0> N; 
  int<lower=0> n_cntry; 
  vector<lower=0,upper=1>[N] unemp;
  vector<lower=0,upper=1>[N] female;
  int<lower=1,upper=n_cntry> cntry[N];
  int<lower=0,upper=1> y[N];
  vector<lower=0>[N] foreignpct;
} 
parameters {
  vector[n_cntry] a;
  vector[3] b;
  real<lower=0,upper=100> sigma_a;
  real mu_a;
}
transformed parameters {
  vector[N] y_hat;
  for (i in 1:N)
    y_hat[i] <- b[1] * unemp[i] + b[2] * female[i] + b[3]  * foreignpct[i]+ a[cntry[i]];
} 
model {
  mu_a ~ normal(0, 1);
  a ~ normal (mu_a, sigma_a);
  b ~ normal (0, 100);
  y ~ bernoulli_logit(y_hat);
}