library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(mxmaps)
library(estcomp)


df_mxmunicipio <- as_tibble(df_mxmunicipio)

# Crea un subconjunto de los datos df_mxmunicipio que contenga únicamente los municipios de
# la CDMX (state_abbr es CDMX)

# Los municipios de Nuevo León con más de 200,000 habitantes.

unique(df_mxmunicipio$state_abbr)

filter(df_mxmunicipio, state_abbr == 'NL', pop >= 200000)

# Los municipios donde más de la mitad la población se autoidentifica como afromexicana
# o parte afromexicana.

filter(df_mxmunicipio, pop/2 < (afromexican + part_afromexican))

# Ve la ayuda de select (?select) y escribe tres maneras de seleccionar las variables
# del estado en los datos df_mxmunicipio.

select(df_mxmunicipio, starts_with("state"))
select(df_mxmunicipio, contains("state"))
select(df_mxmunicipio, matches("(state)"))
select(df_mxmunicipio, 1,4:7)

# Ordena los municipios por población, de mayor a menor.

arrange(select(df_mxmunicipio, state_name, municipio_name, pop), desc(pop))

# ¿Cuáles son los municipios con mayor disparidad de sexo (a total)?

arrange(select(df_mxmunicipio, state_name, municipio_name, pop, pop_male, pop_female), desc(abs(pop_male-pop_female)))
  
# ¿Cuáles son los municipios con mayor disparidad de sexo (proporcional)?,
# elimina los municipios con menos de 5000 habitantes y repite.

arrange(select(df_mxmunicipio, state_name, municipio_name, pop, pop_male, pop_female), desc(abs(pop_male-pop_female)/pop))
filter(arrange(select(df_mxmunicipio, state_name, municipio_name, pop, pop_male, pop_female), desc(abs(pop_male-pop_female)/pop)), pop > 5000)

# Calcula el porcentaje de población indígena de cada municipio y almacenalo en una nueva variable.

select(mutate(df_mxmunicipio, pob_indigena = part_indigenous/pop*100), state_name, municipio_name, pop, part_indigenous, pob_indigena)

#  Crea una nueva variable que muestre el cociente entre la población femenina y masculina

select(mutate(df_mxmunicipio, coc_f_m = pop_female/pop_male), state_name, municipio_name, pop, pop_female, pop_male, coc_f_m)

# Calcula la población total por estado.

by_state <- group_by(df_mxmunicipio, state_name)
summarize(by_state, pob = sum(pop))

# Calcula la población indígena y afromexicana por estado

summarize(by_state, pob_ind = sum(indigenous), pop_afro = sum(afromexican))


# Siguiendo con los datos election_2012, ¿Qué estados tienen la mayor participación
# (esto es del total de votantes en la lista nominal que porcentaje asistió a votar)?
# Tip: debes eliminar las casillas especiales pues la lista nominal (ln) no está definida


election_2012 %>% 
  filter(polling_type != 'S') %>% 
  group_by(state_abbr) %>% 
  summarize(total = sum(total), nominal_list = sum(nominal_list)) %>% 
  mutate(part = total/nominal_list*100) %>% 
  arrange(desc(part))

election_2012 %>% 
  filter(polling_type != 'S') %>% 
  group_by(state_abbr) %>% 
  summarize(part = (sum(total)/sum(nominal_list)*100)) %>% 
  arrange(desc(part))
