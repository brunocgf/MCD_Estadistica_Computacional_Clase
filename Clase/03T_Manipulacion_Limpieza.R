library(estcomp)
library(tidyverse)


# 1. ¿Están limpios los datos? en caso de que no ¿qué principio no cumplen?

# 2. Limpia los datos y muestra las primeras y últimas líneas (usa `head()` y `tail()`)
  
print(df_marital, n = 10)

df_ml <- df_marital %>% 
  gather(key = 'edo_civil', value = 'porc', single:other) %>% 
  mutate(Total = round(porc*pop/100, 0)) %>% 
  select(-pop, -porc)

# 3. Filtra para eliminar los casos a total en las variables sexo y edad,
# calcula a nivel nacional cuál es la proporción en cada situación conyugal por grupo de edad y sexo.
# ¿Cómo puedes graficar o presentar los resultados?

df_mg <- df_ml %>% 
  filter(sex != 'Total', age_group != 'Total') %>% 
  group_by(sex, age_group, edo_civil) %>% 
  summarize(total = sum(Total)) %>% 
  ungroup() %>% 
  group_by(sex, age_group) %>% 
  mutate(proporcion = total/sum(total)*100, etiqueta = ifelse(proporcion>3, paste(round(proporcion,0), "%"), ""))
  
ggplot(df_mg, aes(x = age_group, y = proporcion, fill = edo_civil)) +
  geom_bar(stat = 'identity', position = 'stack') +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            size = 3) +
  labs(title = "Estado civil por sexo y rango de edad",
       fill = "Estado Civil",
       x = element_blank(),
       y = element_blank()) +
  facet_grid(sex~.) +
  theme_light()

ggplot(df_mg) +
  geom_bar(aes(x = age_group, y = proporcion, fill = edo_civil), stat = 'identity', position = 'stack') +
  facet_grid(.~sex) +
  coord_polar("y") +
  labs(title = "Estado civil por sexo y rango de edad",
       fill = "Estado Civil",
       x = element_blank(),
       y = element_blank()) +
  theme_light()


# 4. Regresando a los datos que obtuviste en 2, une la tabla de datos con `df_edu`,
# ¿qué variables se usarán para unir?

glimpse(df_edu)

df_mar_edu <- left_join(df_ml, df_edu)
