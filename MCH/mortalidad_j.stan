data {
  int<lower=0> N;
  vector<lower=0>[N] e;
  int<lower=0> y[N];
}

parameters {
  vector<lower=0,upper=1>[N] lambda;
  real<lower=0> mu;
  real<lower=0> alpha;
}
transformed parameters {
  vector<lower=0>[N] lambda2;
  real<lower=0> beta;
  beta = alpha / mu;
  lambda2 = lambda .* e;
}
model {
  lambda ~ gamma(alpha,beta);
  y ~ poisson(lambda2);
  mu ~ normal(0,1);
  alpha ~ normal(0,10);
  
}

