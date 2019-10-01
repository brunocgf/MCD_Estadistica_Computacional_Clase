library(bootstrap)
library(tidyverse)

# Comapara el intervalo anterior con los intervalos normal y de percentiles.

v <- sum((spatial$A - mean(spatial$A)) ^ 2) / nrow(spatial)

var_sesgada <- function(x) sum((x - mean(x)) ^ 2) / length(x)

var_muestra <- function(x){
  n <- length(x)
  muestra_boot <- sample(x, size = n, replace = TRUE)
  var_sesgada(muestra_boot)
}


m <- rerun(2000, var_muestra(spatial$A)) %>% flatten_dbl()

se <- sd(m)

#Intervalo normal
c(v+se*qnorm(0.025),v+ se*qnorm(0.975))

?#Intervalo quantil
quantile(m, c(0.025, 0.975))

#Bootstrap muestreo complejo

library(usethis)
library(here)


use_zip("https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/datosabiertos/conjunto_de_datos_enigh_2018_ns_csv.zip", "data")

concentrado_hogar <- read_csv(here("data", 
                                   "conjunto_de_datos_enigh_2018_ns_csv", 
                                   "conjunto_de_datos_concentradohogar_enigh_2018_ns", "conjunto_de_datos",
                                   "conjunto_de_datos_concentradohogar_enigh_2018_ns.csv"))

hogar <- concentrado_hogar %>% 
  mutate(
    upm = as.integer(upm),
    jefe_hombre = sexo_jefe == 1, 
    edo = str_sub(ubica_geo, 1, 2), 
    jefa_50 = (sexo_jefe == 2) & (edad_jefe > 50)
  ) %>% 
  select(folioviv, foliohog, est_dis, upm, factor, ing_cor, sexo_jefe, 
         edad_jefe, edo, jefa_50) %>% 
  group_by(est_dis) %>% 
  mutate(n = n_distinct(upm)) %>% # número de upms por estrato
  ungroup()

# creamos una tabla con los estratos y upms
est_upm <- hogar %>% 
  distinct(est_dis, upm, n)

hogar_factor <- est_upm %>% 
  split(.$est_dis) %>% # dentro de cada estrato tomamos muestra (n_h-1)
  map_df(~sample_n(., size = first(.$n) - 1, replace = TRUE)) %>% 
  add_count(upm, name = "m_hi") %>% # calculamos m_hi*
  left_join(hogar, by = c("est_dis", "upm", "n")) %>% 
  mutate(factor_b = factor * m_hi * n / (n - 1))

# unimos los pasos anteriores en una función para replicar en cada muestra bootstrap
svy_boot <- function(est_upm, hogar){
  m_hi <- est_upm %>% 
    split(.$est_dis) %>% 
    map(~sample(.$upm, size = first(.$n) - 1, replace = TRUE)) %>% 
    flatten_int() %>% 
    plyr::count() %>% 
    select(upm = x, m_h = freq)
  m_hi %>% 
    left_join(hogar, by = c("upm")) %>% 
    mutate(factor_b = factor * m_h * n / (n - 1))
}

m_hi <- est_upm %>% 
  split(.$est_dis) %>% 
  map(~sample(.$upm, size = first(.$n) - 1, replace = TRUE)) %>% 
  flatten_int() %>% 
  plyr::count() %>% 
  select(upm = x, m_h = freq)



set.seed(1038984)
boot_rep <- rerun(500, svy_boot(est_upm, hogar))

# Aplicación a ingreso medio
wtd_mean <- function(w, x, na.rm = FALSE) {
  sum(w * x, na.rm = na.rm) / sum(w, na.rm = na.rm)
} 

# La media es:
hogar %>% 
  summarise(media = wtd_mean(factor, ing_cor))

map_dbl(boot_rep, ~wtd_mean(w = .$factor_b, x = .$ing_cor)) %>% sd()
