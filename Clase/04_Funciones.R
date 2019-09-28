library(estcomp)
library(tidyverse)
library(readxl)
library(purr)

# Calcula el valor máximo de cada columna numérica de los datos de ENLACE 3o de primaria `enlacep_2013_3`

glimpse(enlacep_2013_3)

r <- vector('double', 5)

for (i in 1:5){
  
  r[i] <- max(enlacep_2013_3[[i+1]], na.rm = TRUE)  
  
}

library(usethis)
use_directory("data") # crea carpeta en caso de que no exista ya
usethis::use_zip("https://github.com/tereom/estcomp/raw/master/data-raw/19RAMA.zip", 
                 "data") # descargar y descomprimir zip

paths <- dir("data/19RAMA", pattern = "\\.xls$", full.names = TRUE)


for (i in seq_along(paths)){
  data <- read_excel(paths[i])
  data$contaminate <- basename(paths[i])
  final <- rbind(final, data)
}

#names(final) <- basename(paths)
#bind_rows(final, .id = 'filenam')



map_dbl(enlacep_2013_3[,-1], max, na.rm = TRUE)

paths <- set_names(paths, basename(paths))
rama <- map_df(paths, read_excel, .id = "FILENAME")


# Usa la función map_** para calcular el número de valores únicos en las columnas de iris.

map_int(iris, n_distinct)
map_int(iris, function(x)length(unique(x)))
map_int(iris, ~length(unique(.)))

# Usa la función map_** para extraer el coeficiete de la variable wt para cada modelo:
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))

map_dbl(models, ~.$coefficients[2])
