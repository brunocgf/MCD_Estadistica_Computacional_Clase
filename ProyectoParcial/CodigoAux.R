library(FactoMineR)
library(tidyverse)

data(tea)


tabla <- tea %>% 
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop_price = (100 * n / sum(n))) %>% 
  group_by(price) %>% 
  mutate(prom_prop = mean(prop_price)) %>% 
  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2))  


tabla_perfil <- tabla %>%   
  select(how, price, perfil) %>% 
  spread(how, perfil, fill = -1) 


#Segunda forma (solo como ejercicio, no examen)

tabla2 <- tea %>% 
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop_price = (100 * n / sum(n))) %>% 
  ungroup() %>% 
  mutate(prom_prop = mean(prop_price)) %>% 
  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2)) 

tabla_perfil2 <- tabla2 %>%   
  select(how, price, perfil) %>% 
  spread(how, perfil, fill = -1) 


# 1. Utiliza bootstrap para crear intervalos de confianza sobre los perfiles de la última tabla.

# función bootstrap
perfiles_boot <- function(x){
  m <- sample_n(x, size =  300 , replace = TRUE)
  tabla <- m %>% 
    count(how, price) %>% 
    group_by(how) %>% 
    mutate(prop_price = (100 * n / sum(n))) %>% 
    group_by(price) %>% 
    mutate(prom_prop = mean(prop_price)) %>% 
    mutate(perfil = (prop_price / prom_prop - 1) %>% round(2))
  tabla
}

# Repeticiones

perfiles_rep <- rerun(1000, perfiles_boot(tea)) %>% map_dfr(~.x)


# Error estandard

perfiles_se <- perfiles_rep %>% 
  group_by(how, price) %>% 
  summarise(se = sd(perfil))


# Intervalos

perfiles_int <- tabla %>% 
  left_join(perfiles_se) %>% 
  mutate(Int_inf = perfil+qnorm(0.025)*se, Int_sup = perfil+qnorm(0.975)*se)
