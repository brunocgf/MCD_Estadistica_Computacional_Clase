library(tidyverse)
library(scales)

modelo_ahorro <- function(){
  
  ahorro <- rnorm(1,5,4) %>% 
    max(0)
  mercado <- rnorm(1,40000,10000)
  
  ahorro*mercado
  
}

ahorro <- rerun(10000, modelo_ahorro()) %>% flatten_dbl() %>% as_tibble()

ggplot(ahorro) +
  geom_histogram(aes(value), bins = 20) +
  scale_x_continuous(breaks = seq(0,1000000,200000), labels = unit_format(unit = ".", scale = 1)) +
  theme_light()

ahorro_medio <- mean(ahorro)
ahorro_desv <- sd(ahorro)

ahorro_intervalo <- ahorro_medio+ahorro_desv*c(qnorm(0.05),qnorm(0.95))

map_dbl(ahorro_intervalo,max,0)

seq(0,100000,20000)
