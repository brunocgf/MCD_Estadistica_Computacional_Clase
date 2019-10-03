library(FactoMineR)
library(tidyverse)



# Ejercicio 1 -------------------------------------------------------------

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

#tabla2 <- tea %>% 
#  count(how, price) %>% 
#  group_by(how) %>% 
#  mutate(prop_price = (100 * n / sum(n))) %>% 
#  ungroup() %>% 
#  mutate(prom_prop = mean(prop_price)) %>% 
#  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2)) 

#tabla_perfil2 <- tabla2 %>%   
#  select(how, price, perfil) %>% 
#  spread(how, perfil, fill = -1) 


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

ggplot(perfiles_int) +
  geom_segment(aes(y = price, yend = price, x = Int_inf, xend = Int_sup), size = 1) +
  geom_point(aes(x = perfil, y = price), size = 2) +
  facet_wrap(how~.) +
  labs(x = element_blank(),
       y = element_blank()) +
  theme_light()


# Ejercicio 2 -------------------------------------------------------------

library(estcomp)
# universo
enlace <- enlacep_2013 %>% 
  janitor::clean_names() %>% 
  mutate(id = 1:n()) %>% 
  select(id, cve_ent, turno, tipo, esp_3 = punt_esp_3, esp_6 = punt_esp_6, 
         n_eval_3 = alum_eval_3, n_eval_6 = alum_eval_6) %>% 
  na.omit() %>% 
  filter(esp_3 > 0, esp_6 > 0, n_eval_3 > 0, n_eval_6 > 0, cve_ent == "15")
set.seed(16021)
n <- 300
# muestra
enlace_muestra <- sample_n(enlace, n) %>% 
  mutate(clase = "muestra")

median(enlace$esp_3)

# 1. Crea un intervalo del 90% para $\hat{\theta}$ usando los percentiles de la distribución bootstrap, y $B=100$ replicaciones.

enlace_boot <- function(x,col){
  col <- enquo(col)
  n <- nrow(x)
  muestra <- sample_n(x,n, replace = TRUE)
  muestra %>% 
    select(!!col) %>% 
    unlist() %>% 
    median()
}


enlace_rep <- rerun(100, enlace_boot(enlace,esp_3)) %>% flatten_dbl()

quantile(enlace_rep, c(0.025, 0.975))


# 2. Podemos estimar el error estándar de Monte Carlo de los extremos de los intervalos (percentiles 0.05 y 0.95) haciendo bootstrap de la distribución bootstrap:
# Selecciona muestras con reemplazo de tamaño $B$ de la distribución bootstrap
#   + Calcula los percentiles de interés (0.05 y 0.95)

# Construimos la función bootstrap de la distribución bootstrap

enlace_boot_boot <- function(x){
  n <- length(x)
  muestra <- sample(x, size = n, replace = TRUE)
  tibble(Int_inf = quantile(muestra, c(0.025,0.975))[1], Int_sup = quantile(muestra, c(0.025,0.975))[2])
}

#Obtenemos las repeticiones
enlace_boot_rep <- rerun(1000,enlace_boot_boot(enlace_rep)) %>% map_dfr(~.x)

# + Calcula la desviación estándar de los percentiles (una para cada extremo), esta será tu aproximación al error de Monte Carlo

enlace_boot_sd <- map_dbl(enlace_boot_rep, sd)

