data {
    int N;
    int y[N];
    int nExp;
    int Exp[N];
}
parameters {
    real<lower=0,upper=1> theta[nExp];
    real<lower=0,upper=1> mu;
    real<lower=0>  kappa;
}
transformed parameters {
    real<lower=0>  a;
    real<lower=0>  b;
    a = mu * kappa;
    b = (1-mu) * kappa;
}
model {
    theta ~ beta(a,b);
    y ~ bernoulli(theta[Exp]);
    mu ~ beta(10, 10);
    kappa ~ gamma(0.51, 0.01);
}
