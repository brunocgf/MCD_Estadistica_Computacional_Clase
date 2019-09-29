library(estcomp)
library(tidyverse)

set.seed(1983)
enlace_sample <- enlacep_2013 %>% 
  janitor::clean_names() %>% 
  mutate(id = 1:n()) %>% 
  select(id, cve_ent, turno, tipo, mat_3 = punt_mat_3, 
         n_eval_3 = alum_eval_3) %>% 
  na.omit() %>% 
  filter(mat_3 > 0, n_eval_3 > 0) %>% 
  group_by(cve_ent) %>% 
  sample_frac(size = 0.1) %>% 
  ungroup()
       
# - Selecciona el subconjunto de datos de Chiapas (clave de entidad 07):

filter(enlacep_2013, NOM_ENT == 'CHIAPAS')
   
#   + Calcula el estimador plug-in para la mediana de las calificaciones de 
# matemáticas (en Chiapas).

enlace_chiapas <- enlace_sample %>% 
  filter(cve_ent == '07')

ggplot(enlace_chiapas) +
  geom_histogram(aes(mat_3))

med_chiapas <- median(enlace_chiapas$mat_3)
med_chiapas
# 
# + Calcula el estimador bootstrap del error estándar y construye un intrvalo 
# de confianza normal. Debes 1) tomar muestras bootstrap con reemplazo del 
# subconjunto de datos de Chiapas, 2) calcular la mediana en cada una de las 
# muestras y 3) calcular la desviación estándar de las medianas de 2).

boot_mediana <- function(x){
  n <- length(x)
  m <- sample(x, size = n, replace = TRUE)
  median(m)
}

boot_med_chiapas <- rerun(1000, boot_mediana(enlace_chiapas$mat_3)) %>% flatten_dbl()

se_chiapas <- sd(boot_med_chiapas)

enlace_cdmx <- enlace_sample %>% 
  filter(cve_ent == '09')

med_cdmx <- median(enlace_cdmx$mat_3)

boot_med_cdmx <- rerun(1000, boot_mediana(enlace_cdmx$mat_3)) %>% flatten_dbl()

se_cdmx <- sd(boot_med_cdmx)


int_chiapas <- c(med_chiapas+qnorm(0.025)*se_chiapas, med_chiapas+qnorm(0.975)*se_chiapas)
int_cdmx <- c(med_cdmx+qnorm(0.025)*se_cdmx, med_cdmx+qnorm(0.975)*se_cdmx)

data.frame(int_chiapas, int_cdmx, row.names = c('Inf','Sup')) %>% t(.)



# 2. Intervalos de confianza. En este ejercicio compararemos distintos intervalos
# de confianza para las medias de una exponencial
# 
# + Simula una muestra de tamaño 40 de una distribución exponencial(1/2).

muestra_exp <- rexp(40, 1/2)
exp_lambda <- 1/mean(muestra_exp)

boot_lambda <- function(x){
  n <- length(x)
  m <- sample(x, size = n, replace = TRUE)
  1/mean(m)
}

boot_exp_lambda <- rerun(1000, boot_lambda(muestra_exp)) %>% flatten_dbl()


library(boot)

library(boot)

boot_lambda <- function(x, ind){
  1/mean(x[ind])
}

boot_sim_exp <- boot(muestra_exp, f_lamda, R = 10000)

ints <- boot.ci(boot_sim_exp, type = c("norm", "perc", "bca"))
boot_200 <- map(1:200, ~boot(muestra_exp, boot_lambda, R = 10000))

ints_200 <- map(1:200, ~boot(muestra_exp, boot_lambda, R = 10000)) %>% 
  map(boot.ci, type = c("norm", "perc", "bca"))

ints_norm <- map(ints_200, ~.$normal) %>%
  flatten_dbl() %>%
  matrix(ncol = 3, byrow = TRUE) %>% 
  .[,2:3] %>% 
  as.data.frame()

ints_perc <- map(ints_200, ~.$percent) %>%
  flatten_dbl() %>%
  matrix(ncol = 5, byrow = TRUE) %>% 
  .[,4:5]%>% 
  as.data.frame()

ints_bca <- map(ints_200, ~.$bca) %>%
  flatten_dbl() %>%
  matrix(ncol = 5, byrow = TRUE) %>% 
  .[,4:5]%>% 
  as.data.frame()

ints_norm$tipo <- 'normal'
ints_perc$tipo <- 'percentil'
ints_bca$tipo <- 'bca'

ints <- rbind(ints_norm,ints_perc,ints_bca)
names(ints) <- c('Infe','Sup','Tipo')

ggplot(ints) +
  geom_point(aes(x = Infe, y = Sup, color = Tipo)) +
  labs(x = 'Inferior',
       y = 'Superior') +
  theme_light()
