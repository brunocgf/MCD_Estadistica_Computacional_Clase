
library(tidyverse)
library(estcomp)


enlace <- enlacep_2013 %>% 
  janitor::clean_names() %>% 
  mutate(id = 1:n()) %>% 
  select(id, cve_ent, turno, tipo, esp_3 = punt_esp_3, esp_6 = punt_esp_6, 
         n_eval_3 = alum_eval_3, n_eval_6 = alum_eval_6) %>% 
  na.omit() %>% 
  filter(esp_3 > 0, esp_6 > 0, n_eval_3 > 0, n_eval_6 > 0, cve_ent == "15")

enlace_muestra <- sample_n(enlace, 300) %>% 
  mutate(clase = "muestra")


mediaBoot <- function(x){ 
  # x: variable de interés
  # n: número de replicaciones bootstrap
  n <- length(x)
  muestra_boot <- sample(x, size = n, replace = TRUE)
  mean(muestra_boot) # replicacion bootstrap de theta_gorro
}
thetas_boot <- rerun(10000, mediaBoot(enlace_muestra$esp_3)) %>% flatten_dbl()
sd(thetas_boot)

se <- function(x) sqrt(sum((x - mean(x)) ^ 2)) / length(x)
se(enlace_muestra$esp_3)

corBoot <- function(df){ 
  n <- nrow(df)
  muestra_boot <- sample_n(df, size = n, replace = TRUE)
  cor(muestra_boot)[2,1] # replicacion bootstrap de theta_gorro
}

cor_boot <- rerun(10000, corBoot(enlace_muestra[,5:6])) %>% flatten_dbl()s
sd(cor_boot)
