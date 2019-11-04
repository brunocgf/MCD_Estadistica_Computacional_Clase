library(tidyverse)
library(ggplot2)
library(gridExtra)

mu <- 5
theta <- exp(mu)
muestra <- rnorm(100, mu, 1)
mu_est <- (mean(muestra))
sd_est <- sd(muestra)

se_tetha <-  exp(mu_est)/sqrt(100)

theta+se_tetha*c(qnorm(0.025), qnorm(0.975))

sim_t <- rnorm(1000,exp(mu_est), se_tetha)

theta_bootp <- function(){
  m <- rnorm(100, mu_est, sd_est)
  mu_boot <- mean(m)
  sd_boot <- sd(m)
  exp(mu_boot)
}

sim_bootp <- rerun(1000, theta_bootp()) %>% flatten_dbl()
se_bootp <- sd(sim_bootp)
theta+se_bootp*c(qnorm(0.025), qnorm(0.975))

theta_bootnp <- function(){
  m <- sample(muestra, 100, replace = TRUE)
  mu_boot <- mean(m)
  sd_boot <- sd(m)
  exp(mu_boot)
}

sim_bootnp <- rerun(1000, theta_bootnp()) %>% flatten_dbl()
se_bootnp <- sd(sim_bootnp)
theta+se_bootnp*c(qnorm(0.025), qnorm(0.975))


sim <- function(){
  m <- rnorm(100, mu, 1)
  exp(mean(m))
}

simn <- rerun(1000, sim()) %>% flatten_dbl()

df <- tibble(Simulacion = sim_bootp, Metodo = 'Parametrico') %>% 
  rbind(tibble(Simulacion = sim_bootnp, Metodo = 'No Parametrico')) %>% 
  rbind(tibble(Simulacion = sim_t, Metodo = 'Delta')) %>% 
  rbind(tibble(Simulacion = simn, Metodo = 'Simulacion'))

ggplot(df) +
  geom_histogram(aes(Simulacion)) +
  facet_grid(Metodo~.) +
  theme_light()
