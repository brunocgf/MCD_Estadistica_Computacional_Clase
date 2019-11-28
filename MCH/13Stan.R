library(rstan)

# opcional para correr en paralelo
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Modelo Bernoulli
stan_cpp <- stan_model('modelo_bernoulli.stan')

N = 50 ; z = 10 ; y = c(rep(1, z), rep(0, N - z))

data_list <- list(y = y, N = N )
stan_fit <- sampling(object= stan_cpp, data = data_list,
                     chains = 3 , iter = 1000 , warmup = 200, thin = 1 )
summary_stan_fit <- summary(stan_fit)

traceplot(stan_fit)

stan_fit_2 <- stan(file = 'modelo_bernoulli.stan',
                   data = data_list,
                   iter = 1000,
                   chains = 4)

# Modelo Normal

stan_cpp2 <- stan_model('modelo_normal.stan')

N <- 50
set.seed(122)
y <- rnorm(N,2,2)
data_list <- list(y = y, N = N)

stan_fit_norm <- sampling(object= stan_cpp2,
                      data = data_list,
                      iter = 1000,
                      chains = 3,
                      warmup = 500)

# Realiza un histograma de la distribución predictiva posterior.
# Construye un intervalo del 95% de probabilidad para una predicción
# Tip: usa la función extract()

library(tidyverse)

par_sim_norm <- extract(stan_fit_norm) %>% unlist()
par_sim_mean <- par_sim_norm$mu
par_sim_s <- par_sim_norm$sigma2 %>% sqrt()

dist_pp <- rnorm(1500, par_sim_mean, par_sim_s)

hist(dist_pp)

quantile(dist_pp, c(0.025,0.975))

# Graficas

library(bayesplot, verbose = FALSE)

norm_posterior_inc_warmup <- extract(norm_fit,
                                     inc_warmup = TRUE)


shinystan::