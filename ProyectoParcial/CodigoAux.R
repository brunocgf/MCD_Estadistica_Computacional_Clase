library(FactoMineR)
library(tidyverse)

data(tea)

tea <- tea %>% 
  as_tibble %>% 
  select(how, price, sugar)

tipo <- tipo %>% select(how, prop_how = `%`)
tabla_2 <- tea %>%
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop = round(100 * n / sum(n))) %>% 
  select(-n) 
tabla_2 %>% spread(how, prop, fill = 0) %>% formatear_tabla()

tabla <- tea %>% 
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop_price = (100 * n / sum(n))) %>% 
  group_by(price) %>% 
  mutate(prom_prop = mean(prop_price)) %>% 
  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2))  

precio_prom <- tabla %>% 
  distinct(price, prom_prop) %>%
  mutate(promedio = round(prom_prop)) %>% 
  select(price, promedio)
tabla_perfil <- tabla %>%   
  select(how, price, perfil) %>% 
  spread(how, perfil, fill = -1) 
tabla_2 <- tabla_perfil %>% 
  gather(how, prop_price, -price)
if_profile <- function(x){
  any(x < 0) & any(x > 0)
}
marcar_tabla_fun <- function(corte, color_1 = "darkgreen", color_2 = "red"){
  fun_marcar <- function(x){
    kableExtra::cell_spec(x, "html", 
                          color = ifelse(x <= -corte, color_1, 
                                         ifelse(x >= corte, color_2, "lightgray")))   
  }
  fun_marcar
}
marcar <- marcar_tabla_fun(0.25, "red", "black")
tab_out <- tabla_perfil %>% left_join(precio_prom) %>%
  arrange(desc(`tea bag`)) %>% 
  #mutate_if(if_profile, marcar) %>% 
  knitr::kable(format = "html", escape = F, digits = 2) %>% 
  kableExtra::kable_styling(bootstrap_options = c( "hover", "condensed"), 
                            full_width = FALSE)
tab_out
