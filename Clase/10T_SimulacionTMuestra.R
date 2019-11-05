library(tidyverse)
library(ggplot2)
library(gridExtra)


# Muestra -----------------------------------------------------------------

J = 10

muestreo <- function(J, n_total = 1000) {
  n_cong <- floor(n_total / J)
  medias <- rnorm(J, 0.5, 0.1)
  medias <- case_when(
    medias < 0 ~ 0, 
    medias > 1 ~ 1,
    TRUE ~ medias)
  resp <- rbinom(J, n_cong, medias)
  sum(resp) / n_total
}
errores <- tibble(J = c(1, 10, 100, 1000)) %>% 
  mutate(
    sims = map(J, ~(rerun(1000, muestreo(.)) %>% flatten_dbl())), 
    error_est = map_dbl(sims, sd) %>% round(3)
  )
errores
tamano_muestra <- function(J) {
  n_total <- max(100, J)
  ee <- rerun(1000, muestreo(J = J, n_total = n_total)) %>% 
    flatten_dbl() %>% sd()
  while(ee > 0.02){
    n_total = n_total + 20
    ee <- rerun(500, muestreo(J = J, n_total = n_total)) %>% 
      flatten_dbl() %>% 
      sd() %>% 
      round(3)
  }
  list(ee = ee, n_total = n_total, costo = 500 * J + 50 * n_total)
}
tamanos <- c(20, 30, 40, 50, 100, 150)
costos <- map_df(tamanos, tamano_muestra)
costos$J <- tamanos
costos
ggplot(costos, aes(x = J, y = costo / 1000)) +
  geom_line() + scale_y_log10() + theme_minimal() +
  labs(y = "miles de pesos", title = "Costos")


# Bootstrap ---------------------------------------------------------------



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
