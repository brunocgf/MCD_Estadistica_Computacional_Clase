#devtools::install_github("tereom/estcomp")

library(estcomp)
library(tidyverse)
library(mxmaps)


glimpse(df_edu)
str(df_edu)


# ¿Cuál es el municipo con mayor escolaridad promedio (valor de `schoolyrs`)?
# Tip: usa `filter` para quedarte únicamente con `sex` correspondiente a `Total`.


df_edu %>%
  filter(sex == "Total") %>% 
  select(state_code, municipio_code, state_name, state_abbr, municipio_name, schoolyrs) %>% 
  arrange(-schoolyrs)

  
# Crea una `data.frame` que contenga una línea por cada estado y por sexo, con la siguiente información:

#   + la escolaridad promedio por estado y sexo (ponderada por la población `pop_15`)
#   + la población de cada sexo (mayor a 15 años)

df_edu %>% 
  filter(sex != 'Total', !is.na(schoolyrs)) %>% 
  group_by(state_code, state_name, sex) %>%
  summarize(poblacion = sum(pop_15), escolaridad_prom = sum(schoolyrs*pop_15)/sum(pop_15))

# Crea una variable que indique el porcentaje de la población que cursó al menos educación básica.

df_Meb <- df_edu %>% 
  filter(sex == 'Total') %>% 
  mutate(porcentaje_Meb = (elementary + secondary + highschool + higher_edu)/100, poblacion_Meb = porcentaje_Meb*pop_15) %>%
  summarize(total_Meb = 100*sum(poblacion_Meb)/sum(pop_15))

# Enuncia al menos una pregunta que se pueda responder transformando y graficando estos datos. Crea tu(s) gráfica(s).

# ¿Que diferencia existe en la escolaridad entre la población masculina y la población femenina?

df_dif <-  df_edu %>%
  filter(sex != 'Total', !is.na(schoolyrs)) %>%
  select(state_code, municipio_code, state_name, municipio_name, sex, pop_15, schoolyrs) %>% 
  unite(temp, pop_15, schoolyrs) %>% 
  spread(key = sex, value = temp) %>% 
  separate(Hombres, into = c("pop_H", "esc_H"), sep = "_") %>%
  separate(Mujeres, into = c("pop_M", "esc_M"), sep = "_") %>%
  mutate(esc_M = as.numeric(esc_M), 
         pop_H = as.numeric(pop_H), 
         pop_M = as.numeric(pop_M), 
         esc_H = as.numeric(esc_H),
         diferencia = esc_M - esc_H,
         mayor_esc = ifelse(diferencia<=0, "H", "M"))

ggplot(df_dif) +
  geom_point(aes(x = esc_H, y = esc_M, color = mayor_esc), size = 1, alpha = 0.8) +
  coord_cartesian(xlim = c(2,14), ylim = c(2,14)) +
  scale_x_continuous(breaks = seq(2,14,2)) +
  scale_y_continuous(breaks = seq(2,14,2)) +
  labs(title = "Escolaridad por género por municipio",
       x = "Hombres",
       y = "Mujeres",
       color = "Mayor escolaridad") +
  theme_light()

EP_H <- sum(df_dif$pop_H*df_dif$esc_H)/sum(df_dif$pop_H)
EP_M <- sum(df_dif$pop_M*df_dif$esc_M)/sum(df_dif$pop_M)

df_map <- df_dif %>% 
  select(region = state_code, value = diferencia)

df_map <-  df_edu %>%
  filter(sex != 'Total', !is.na(schoolyrs)) %>%
  select(region = state_code, municipio_code, state_name, sex, pop_15, schoolyrs) %>% 
  unite(temp, pop_15, schoolyrs) %>% 
  spread(key = sex, value = temp) %>% 
  separate(Hombres, into = c("pop_H", "esc_H"), sep = "_") %>%
  separate(Mujeres, into = c("pop_M", "esc_M"), sep = "_") %>%
  mutate(esc_M = as.numeric(esc_M), 
         pop_H = as.numeric(pop_H), 
         pop_M = as.numeric(pop_M), 
         esc_H = as.numeric(esc_H)) %>% 
  group_by(region, state_name) %>% 
  summarize(esc_M = mean(esc_M), pop_M = sum(pop_M), esc_H = mean(esc_H), pop_H = sum(pop_H)) %>% 
  mutate(value = esc_H - esc_M)



mxstate_choropleth(df_map,
                   num_colors = 4,
                   title = "Diferencia de escolaridad entre Hombres y Mujeres")
