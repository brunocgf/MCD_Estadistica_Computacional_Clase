### 2. Simulación para el cálculo de tamaños de muestra {-}

# En el conteo rápido del estado de Guanajuato, se calculó el tamaño de muestra
# fijando como objetivo que los intervalos del $95$% de confianza tuvieran una 
# longitud máxima de 2 puntos porcentuales para todos los candidatos. En este 
# ejercicio calcularás el tamaño de muestra mínimo que cumpla con el objetivo
# usando 3 diseños de muestreo distintos: 1) muestreo aleatorio simple (MAS), 
# 2) estratificando con distrito local y 3) estratificando con distrito federal.
# 
# Utilizarás simulación y los resultados de las elecciones de gobernador en 
# Guanajuato correspondientes al 2012.
# 
# En el caso de **MAS**, para cada tamaño de muestra 
# $n=50,100,200,300,400,500,600,700$:
#   
#   i. Simula una muestra aleatoria de tamaño $n$.
# 
# ii. Calcula el estimador de razón (correspondiente a muestreo aleatorio simple) 
# para cada candidato:
#   
#   $$\hat{p}=\frac{\sum_{i} Y_{i}}{\sum_i X_{i}}$$
#   
#   donde:
#   
#   * $\hat{p}$ es la estimación de la proporción de votos que recibió el candidato
# en la elección.
# 
# * $Y_{i}$ es el número total de votos que recibió el candidato
# en la $i$-ésima casilla.
# 
# * $X_{i}$ es el número total de votos en la $i$-ésima casilla.  

library(tidyverse)
library(dplyr)
library(glue)

gto_2012 <- read.csv("./data/gto_2012.csv")

set.seed(141394)
estimador<-function(data,size){
  data %>% sample_n(size) %>%
    mutate(muestra=glue("muestra_{size}")) %>%
    summarise(p_pri=sum(pri_pvem)/sum(total),p_pan=sum(pan_na)/sum(total),
              p_prd=sum(prd)/sum(total),p_pt=sum(pt)/sum(total),
              p_mc=sum(mc)/sum(total),p_otros=sum(otros)/sum(total)) %>% 
    select(p_pri,p_pan,p_prd,p_pt,p_mc,p_otros)
}

iii. Repite los pasos i y ii $1000$ veces para estimar el error estándar para 
una muestra de tamaño $n$.


m_50<-rerun(1000,estimador(gto_2012,50)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                 se_mc=sd(p_mc),
                                                                 se_otros=sd(p_otros))
m_100<-rerun(1000,estimador(gto_2012,100)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))
m_200<-rerun(1000,estimador(gto_2012,200)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

m_300<-rerun(1000,estimador(gto_2012,300)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))
m_400<-rerun(1000,estimador(gto_2012,400)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))
m_500<-rerun(1000,estimador(gto_2012,500)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))
m_600<-rerun(1000,estimador(gto_2012,600)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))
m_700<-rerun(1000,estimador(gto_2012,700)) %>% 
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                                                                  se_mc=sd(p_mc),
                                                                  se_otros=sd(p_otros))

Para cada **estratificación** (`distrito_fed_17` y `distrito_loc_17`) y 
tamaño de muestra $n=50,100,200,300,400,500,600,700$:
  
i. Simula una muestra estratificada de tamaño $n$, donde el tamaño de muestra en 
cada estrato se asigna proporcional al tamaño del estrato, esto es, sea $N_h$ el 
número de casillas en el $h$-ésimo estrato, entonces para el estrato $h$ el 
número de casillas en la muestra será:
  $$n_h = N_h \cdot \frac{n}{\sum_j N_j}$$
  
ii. Calcula el estimador de razón combinado (correspondiente a muestreo 
                                               estratificado) para cada candidato:
  
$$\hat{p}=\frac{\sum_h \frac{N_h}{n_h} \sum_i Y_{hi}}{\sum_h \frac{N_h}{n_h} \sum_i X_{hi}}$$

  donde:
  
  * $\hat{p}$ es la estimación de la proporción de votos que recibió el candidato
en la elección.

* $Y_{hi}$ es el número total de votos que recibió el candidato
en la $i$-ésima casillas, que pertence al $h$-ésimo estrato.

* $X_{hi}$ es el número total de votos en la $i$-ésima casilla, que pertence al 
$h$-ésimo estrato. 

* $N_h$ es el número total de casillas en el $h$-ésimo estrato.

* $n_h$ es el número de casillas del $h$-ésimo estrato que se seleccionaron en 
la muestra.

iii. Repite los pasos i y ii $1000$ veces para estimar el error estándar para 
una muestra de tamaño $n$.

####### muestra por estratos ########
by_stratum_dist_17<-gto_2012 %>% 
  group_by(distrito_fed_17) %>% 
  arrange(distrito_fed_17)

by_stratum_local_17<-gto_2012 %>% 
  group_by(distrito_loc_17) %>% 
  arrange(distrito_loc_17)
  
library(sampler)

#Encontramos la n proporcional a cada estrato con las diferentes muestras

size_estrato_fed_17<-ssampcalc(df = gto_2012,n =50,strata = distrito_fed_17) %>%
  cbind(ssampcalc(df = gto_2012,n =100,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =200,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =300,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =400,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =500,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =600,strata = distrito_fed_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n =700,strata = distrito_fed_17)[,4]) 

names(size_estrato_fed_17)<-c("distrito_fed_17","Nh","wt","nh_50","nh_100","nh_200","nh_300","nh_400",
         "nh_500","nh_600","nh_700")

size_estrato_loc_17<-ssampcalc(df = gto_2012,n = 50,strata = distrito_loc_17) %>%
  cbind(ssampcalc(df = gto_2012,n = 100,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 200,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 300,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 400,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 500,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 600,strata = distrito_loc_17)[,4]) %>%
  cbind(ssampcalc(df = gto_2012,n = 700,strata = distrito_loc_17)[,4])

names(size_estrato_loc_17)<-c("distrito_loc_17","Nh","wt","nh_50","nh_100","nh_200","nh_300","nh_400",
                              "nh_500","nh_600","nh_700")
  
library(pps)

#Función Muestra simulada proporcional a los estratos
estimador_strata<-function(stratum,nh,data){
  index_<-stratsrs(stratum,nh)
  data[index_,] %>% 
    summarise(p_pri=sum(pri_pvem)/sum(total),p_pan=sum(pan_na)/sum(total),
                             p_prd=sum(prd)/sum(total),p_pt=sum(pt)/sum(total),
                             p_mc=sum(mc)/sum(total),p_otros=sum(otros)/sum(total)) %>% 
    select(p_pri,p_pan,p_prd,p_pt,p_mc,p_otros)
}

#estimador_strata(stratum = by_stratum_dist_17$distrito_fed_17,nh=size_estrato_fed_17$nh_50,data=gto_2012)


#Estrado distrito federal 17
st1_50<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_50,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
                        se_mc=sd(p_mc),
                        se_otros=sd(p_otros))

st1_100<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_100,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_200<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_200,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_300<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_300,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_400<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_400,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_500<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_500,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_600<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                    size_estrato_fed_17$nh_600,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st1_700<-rerun(1000,estimador_strata(by_stratum_dist_17$distrito_fed_17,
                                     size_estrato_fed_17$nh_700,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

#Estrado distrito local 17

st2_50<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_50,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_100<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_100,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_200<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_200,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_300<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_300,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_400<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_400,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_500<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_500,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_600<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_600,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

st2_700<-rerun(1000,estimador_strata(by_stratum_local_17$distrito_loc_17,
                                    size_estrato_loc_17$nh_700,gto_2012)) %>%
  bind_rows() %>% 
  summarise(se_pri=sd(p_pri),se_pan=sd(p_pan),se_prd=sd(p_prd),se_pt=sd(p_pt),
            se_mc=sd(p_mc),
            se_otros=sd(p_otros))

Ahora:
  
  1. Reporta en una tabla el error estándar para cada candidato, tamaño de muestra
y diseño (MAS y las dos estratificaciones propuestas).

tablaSe<-m_50 %>% 
  rbind(m_100,m_200,m_300,m_400,m_500,m_600,m_700,st1_50,st1_100,st1_200,st1_300,st1_400,st1_500,st1_600,st1_700,
        st2_50,st2_100,st2_200,st2_300,st2_400,st2_500,st2_600,st2_700)
tablaSe$muestra <-c("MA50","MAS100","MAS200","MAS300","MAS400","MAS500","MAS600","MAS700",
                    "1ST50","1ST100","1ST200","1ST300","1ST400","1ST500","1ST600","1ST700",
                    "2ST50","2ST100","2ST200","2ST300","2ST400","2ST500","2ST600","2ST700")
tablaSe


2. Grafica los datos de la tabla: realiza una gráfica de paneles (con `facet_wrap()`), cada partido en un panel, en el eje horizontal grafica el 
tamaño de muestra y en el eje vertical el error estándar, tendrás en una misma 
gráfica tres curvas, una para muestreo aleatorio simple y una para cada estratificación.

library(ggplot2)

tablaSe %>% ggplot() %>% geom_line()


3. ¿Qué diseño y tamaño de muestra elegirías? Explica tu respuesta y de 
ser necesario repite los pasos i-iii para otros valores de $n$.
  

  