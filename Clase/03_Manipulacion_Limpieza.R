library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(mxmaps)
library(estcomp)
library(gridExtra)


# Ahora tu turno, ¿cómo se relacionan los años de escolaridad con el porcentaje de población indígena.
# Utiliza los datos `df_mxmunicipio` y `df_edu` para explorar la relación. ¿cuál es el `join` adecuado?
# ¿de qué tamaño serán los datos finales?

glimpse(df_edu)
glimpse(df_mxmunicipio)

df_edu_pop <- left_join(df_mxmunicipio, df_edu) %>% 
  filter(sex == 'Total')

ggplot(df_edu_pop, aes(x = indigenous/pop, y = schoolyrs)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm")

View(mxstate.map)

election_2012_state <- election_2012 %>%
  group_by(state_code) %>%
  summarise(
    pri_pvem = 100 * sum(pri_pvem) / sum(total),
    pan = 100 * sum(pan) / sum(total),
    prd_pt_mc = 100 * sum(prd_pt_mc) / sum(total)
  ) %>%
  mutate(winner = case_when(
    pri_pvem > pan & pri_pvem > prd_pt_mc ~ "pri_pvem",
    pan > pri_pvem & pan > prd_pt_mc ~ "pan",
    TRUE ~ "prd_pt_mc"), 
    winner_pct = pmax(pri_pvem, pan, prd_pt_mc))


election_map <- mxstate.map %>% 
  left_join(election_2012_state, by = c("region" = "state_code")) 

map_edo <- ggplot(election_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = winner, alpha = winner_pct), color = "#666666", 
               size = .05, show.legend = FALSE) +
  coord_map() +
  scale_fill_manual(values = c("prd_pt_mc" = "#FFCC00", "pan" = "#3399FF", 
                               "pri_pvem" = "#00CD66")) + 
  theme_void()

election_hexbinmap <- mxhexbin.map %>% 
  left_join(election_2012_state, by = c("region" = "state_code"))

state_labels_map <- mxhexbin.map %>% 
  group_by(state_abbr) %>% 
  summarise(long = mean(long), lat = mean(lat), group = first(group))

hexbinmap_edo <- ggplot(election_hexbinmap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = winner, alpha = winner_pct), color = "#666666", 
               size = .05, show.legend = FALSE) +
  coord_map() +
  scale_fill_manual(values = c("prd_pt_mc" = "#FFCC00", "pan" = "#3399FF", 
                               "pri_pvem" = "#00CD66")) +
  geom_text(data = state_labels_map, aes(long, lat, label = state_abbr)) +
  theme_void()

grid.arrange(map_edo, hexbinmap_edo, nrow = 1)

# Genera un mapa a nivel municipo que muestre el porcentaje de la población casada a total (mayores de 12 años).

glimpse(df_marital)
glimpse(mxmunicipio.map)

df_map_marital <- df_marital %>% 
  filter(age_group == '12-17 años') %>% 
  left_join(mxmunicipio.map)

ggplot(df_map_marital, aes(long, lat, group=group))+
  geom_polygon(aes(fill = separated), color = "#666666", size = .02) +
  coord_map() +
  theme_void()

glimpse(pm25_2019)

gather(pm25_2019, key = station, value = measurement, -FECHA, - HORA)

gather(df_edu, key=grade, value = percent, preschool:other) %>% View()

data("enlacep_2013")
enlacep_sub_2013 <- enlacep_2013 %>% 
  select(CVE_ENT:PUNT_FCE_6) %>% 
  sample_n(1000)
glimpse(enlacep_sub_2013)

enlacep_sub_2013 %>% 
  gather(key=area_grado, value = puntaje, PUNT_ESP_3:PUNT_FCE_6) %>% 
  separate(area_grado, c('temp', 'area', 'grado'), sep ='_') %>% 
  View()


data('df_fertility')

fertility_long <- df_fertility %>% 
  gather(key = edad, value = tasa, age_15_19:global)

fertility_vars <- fertility_long %>% 
  mutate(
    state_code = str_sub(state, 1, 2), 
    state_name = str_sub(state, 4)
  ) %>%
  select(-state)

fertility_tider <- spread(fertility_vars, key = est, value = tasa)

# Grafica el valor estimado de fertilidad del grupo de edad 20-24 contra 25-29. ¿Qué transformación debes hacer?

fertility_tider %>% 
  filter(edad == 'age_20_24'|edad == 'age_25_29') %>% 
  select(-`Error estándar`) %>% 
  spread(key = edad, value = Valor) %>% 
  ggplot(aes(x = age_20_24, y = age_25_29)) +
  geom_point()


# Escribe una función que reciba un vector y devuelva el mismo vector reescalado al rango 0 a 1.
# Comienza escribiendo el código para un caso particular, por ejemplo, empieza reescalando el vector.
# Tip: la función range() devuelve el rango de un vector.

f_reescalar <- function(v){
  r <- range(v)
  
  (v-r[1])/(r[2]-r[1])
  
}