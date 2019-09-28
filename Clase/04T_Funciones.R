library(tidyverse)
library(readr)

#1. Descarga la carpeta specdata, ésta contiene 332 archivos csv que almacenan información de monitoreo de contaminación en 332 ubicaciones de EUA. Cada archivo contiene información de una unidad de monitoreo y el número de identificación del monitor es el nombre del archivo. En este ejercicio nos interesa unir todas las tablas en un solo data.frame que incluya el identificador de las estaciones.

# La siguiente instrucción descarga los datos si trabajas con proyectos de RStudio, también puedes descargar el zip manualmente.

#library(usethis)
#use_directory("data") 
#use_zip("https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip", destdir = "data")

# Crea un vector con las direcciones de los archivos. 

paths <- dir("data/specdata", full.names = TRUE)

# Lee uno de los archivos usando la función `read_csv()` del paquete `readr`. Tip: especifica el tipo de cada columna usando el parámetro `col_types`.

read_csv(paths[1],
         col_types = cols(
           Date = col_date(),
           sulfate = col_double(),
           nitrate = col_double(),
           ID = col_integer()
         ))

# Utiliza la función `map_df()` para iterar sobre el vector con las direcciones de los archivos csv y crea un data.frame con todos los datos,
# recuerda añadir una columna con el nombre del archivo para poder identificar la estación.

specdf <- map_df(paths,
          read_csv,
          col_types = cols(Date = col_date(),sulfate = col_double(),nitrate = col_double(),ID = col_integer())
     )


# 2. Consideramos los datos de ENLACE edo. de México (`enlace`),
# y la columna de calificaciones de español 3o de primaria (`esp_3`). 

library(estcomp)
enlace <- enlacep_2013 %>% 
  janitor::clean_names() %>% 
  mutate(id = 1:n()) %>% 
  select(id, cve_ent, turno, tipo, esp_3 = punt_esp_3, esp_6 = punt_esp_6, 
         n_eval_3 = alum_eval_3, n_eval_6 = alum_eval_6) %>% 
  na.omit() %>% 
  filter(esp_3 > 0, esp_6 > 0, n_eval_3 > 0, n_eval_6 > 0, cve_ent == "15")




# Selecciona una muestra de tamaño $n = 10, 100, 1000$. Para cada muestra calcula
# media y el error estándar de la media usando el principio del *plug-in*:
# Tip: Usa la función `sample_n()` del paquete `deplyr` para generar las muestras.


dm_enlace <- function(n){
  men <- sample_n(enlace,n)
  m <- men %>% select(starts_with('esp')) %>% colMeans()
  sde <- men %>% select(starts_with('esp')) %>% map_dbl(sd)/sqrt(n)
  res <- list(media = m, error_estandar = sde)
  return(res)
}

simula_media <- function(n) {
  muestra <- sample_n(enlace,n) %>% 
    gather(grado, calif, esp_3:esp_6)
  mean(muestra$calif)  
}
medias_10 <- rerun(10000, simula_media(n = 10)) %>% flatten_dbl()
medias_100 <- rerun(10000, simula_media(n = 100)) %>% flatten_dbl() 
medias_1000 <- rerun(10000, simula_media(n = 1000)) %>% flatten_dbl()

medias <- tibble(medias_10, medias_100, medias_1000) %>% 
  gather(key = "nsim", "m", 1:3)

ggplot(medias) +
  geom_histogram(aes(m)) +
  facet_grid(.~nsim) +
  labs(title = "Histograma para cada tamaño de muestra",
       x = element_blank(),
       y = element_blank()) +
  theme_light()

vect_es <- enlace %>% 
  gather(grado, calif, esp_3:esp_6) %>% 
  select(calif) %>%
  unlist()

sd(vect_es)/sqrt(length(vect_es))
