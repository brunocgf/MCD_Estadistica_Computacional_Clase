# Elige un base de datos, recuerda que en la ayuda puedes encontrar más información de las variables (?gapminder):
##  gapminder (paquete gapminder en CRAN).
##  election_2012 ó election_sub_2012 (paquete estcomp).
##  df_edu (paquete estcomp).
##  enlacep_2013 o un subconjuto de este (paquete estcomp).

# Escribe algunas preguntas que consideres interesantes de los datos.

# Realiza 3 gráficas buscando explorar las preguntas de arriba y explica las relaciones que encuentres.
# Debes usar lo que revisamos en estas notas y al menos una de las gráficas debe ser de paneles (usando facet_wrap() o facet_grid).

library(tidyverse)
library(gapminder)
library(plotly)

glimpse(gapminder)
summary(gapminder)

data("gapminder")

# ¿Cuál es la relación entre el gdp y la esperanza de vida?

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  theme_light()

# Dada la gran acumulación de datos conviene agruparlos bajo categorías como años o continente

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  theme_light()

# Se nota una mayor relación distinta por cada contienente, sin embargo es posible que no se pueda apreciar la relación en todos
# los continentes dada heterogeneidad de los datos. A continuación se ven los continentes sparados.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  facet_wrap(~continent)

# En el caso de Europa y Oceanía, se ve una relación muy clara.
# En el caso de África, la relación es mucho menor.
# Sin embargo los datos incluyen todos los años, por lo que es probable que no exista causalidad, y en lugar de eso el incremento
# sea natural a través del tiempo.
# A continuación se ven los datos a través de los años.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(color = continent)) +
  facet_wrap(~year)

ggplot(gapminder) +
  geom_line(aes(x = year, y = gdpPercap))

# En cada uno de los años se observa poca correlación en los extremos de los datos, mientras que hay más correlación en el punto medio.

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point() +
  facet_grid(year~continent)

# Al pasar de los años hay continentes que hay aumentado claramente su gdp (como europa y más recientemente asia),
# mientras que hay otros cuyo crecimiento parece ser el mínimo.
# En cambio, la variación en la esperanza de vida parece ser menor

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_boxplot(fill = NA) +
  geom_point(aes(color = continent)) +
  facet_grid(.~year)

# El gráfico muestra in rango semejante de esperanza de vida, sin embargo a pesar de que los puntos se ven más dispuersos a través de los años,
# existe un punto atípico que no permite ver este efecto en la gráfica de caja y brazos.
# Para poder ver con mas detalle vamos a quitar este punto

gapminder[gapminder$gdpPercap == max(gapminder[gapminder$year==1952,]$gdpPercap),]

ggplot(filter(gapminder, country != 'Kuwait'), aes(x = gdpPercap, y = lifeExp)) +
  geom_boxplot(fill = NA, size = 1) +
  geom_point(aes(color = continent)) +
  facet_grid(.~year) +
  theme_minimal()

# Aunque la esperanza ha aumentado a lo largo de los años, no es así con el ingreso de las familias. Por tanto, surge la pregunta,
# ¿ha aumentado en misma medida la calidad de vida?


gapminder_total <- gapminder %>%
  mutate(GDP = gdpPercap*pop/1000000) %>%
  group_by(continent, year) %>%
  summarize(GDP = sum(GDP)) %>%
  arrange(year, GDP) %>%
  #group_by(year) %>%
  #mutate(C_GDP = cumsum(GDP)) %>%
  ungroup()

ggplot(filter(gapminder_total, year == 1962 | year == 1977 | year == 1992 | year == 2007)) +
  geom_bar(aes(x = reorder(continent, GDP), y = GDP, fill = continent), stat = "identity") +
  facet_grid(year~., scales = "free_y")

ggplot(gapminder_total) +
  geom_bar(aes(x = year, y = GDP, fill = continent), stat = 'identity', position = "fill") +
  theme_minimal()
  

