data {
  int n; // número de observaciones
  int n_state; // número de estados
  int n_age;
  int n_edu;
  int n_age_edu;
  int age[n];
  int edu[n];
  int age_edu[n];
  int mh; //número de covariables nivel persona
  int mm; //número de covariables nivel estado
  int<lower=0,upper=1> y[n];
  matrix[n, mh] x_person; // covariables a nivel persona
  int state[n];
  matrix[n_state, mm] x_state; // covariables a nivel estado
} 
parameters {
  real beta_0;
  vector[mh] beta;
  vector[n_state] beta_state_raw;
  vector[n_age] beta_age_raw;
  vector[n_edu] beta_edu_raw;
  vector[n_age_edu] beta_age_edu_raw;
  real<lower=0> sigma_age;
  real<lower=0> sigma_edu;
  real<lower=0> sigma_age_edu;
  real<lower=0> sigma_state;
  vector[mm] alpha;
}
transformed parameters {
  vector[n] reg_pred;
  vector[n_state] beta_state;
  vector[n_age] beta_age;
  vector[n_edu] beta_edu;
  vector[n_age_edu] beta_age_edu;
  // parametrización no centrada
  beta_age = beta_age_raw * sigma_age; 
  beta_edu = beta_edu_raw * sigma_edu; 
  beta_age_edu =beta_age_edu_raw * sigma_age_edu; 
  beta_state = beta_state_raw * sigma_state + x_state * alpha; 
  reg_pred = beta_0 + x_person * beta + beta_state[state] + beta_age[age] + 
    beta_edu[edu] + beta_age_edu[age_edu];
}
model {  
  y ~ bernoulli_logit(reg_pred);
  beta_0 ~ normal(0, 1);
  beta ~ normal(0, 1);
  
  beta_age_raw ~ normal(0, 1);
  beta_edu_raw ~ normal(0, 1);
  beta_age_edu_raw ~ normal(0, 1);
  beta_state_raw ~ normal(0, 1);
  
  sigma_state ~ normal(0, 1);
  sigma_age ~ normal(0, 1);
  sigma_edu ~ normal(0, 1);
  sigma_age_edu ~ normal(0, 1);
  alpha ~ normal(0, 1);
}

