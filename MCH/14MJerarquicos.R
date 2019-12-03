library(LearnBayes)
library(tidyverse)
library(rstan)

data(hearttransplants)
heart <- hearttransplants

heart_deseas <- list(N = 94,
                     e = heart$e,
                     y = heart$y)

model_heart <- stan_model('mortalidad_j.stan')

model_heart_fit <- sampling(object = model_heart,
                            data = heart_deseas,
                            chains = 3 , iter = 1000 , warmup = 200, thin = 1 )
sims_labda <- as.data.frame(model_heart_fit, pars='lambda')
  