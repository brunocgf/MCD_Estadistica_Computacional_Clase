library(FactoMineR)
library(tidyverse)
library(boot)


# Ejercicio 1 -------------------------------------------------------------

data(tea)


tabla <- tea %>% 
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop_price = (100 * n / sum(n))) %>% 
  group_by(price) %>% 
  mutate(prom_prop = mean(prop_price)) %>% 
  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2))  


tabla_perfil <- tabla %>%   
  select(how, price, perfil) %>% 
  spread(how, perfil, fill = -1) 


#Segunda forma (solo como ejercicio, no examen)

#tabla2 <- tea %>% 
#  count(how, price) %>% 
#  group_by(how) %>% 
#  mutate(prop_price = (100 * n / sum(n))) %>% 
#  ungroup() %>% 
#  mutate(prom_prop = mean(prop_price)) %>% 
#  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2)) 

#tabla_perfil2 <- tabla2 %>%   
#  select(how, price, perfil) %>% 
#  spread(how, perfil, fill = -1) 


# 1. Utiliza bootstrap para crear intervalos de confianza sobre los perfiles de la última tabla.

# función bootstrap
perfiles_boot <- function(x){
  n <- nrow(x)
  m <- sample_n(x, size =  n , replace = TRUE)
  tabla <- m %>% 
    count(how, price) %>% 
    group_by(how) %>% 
    mutate(prop_price = (100 * n / sum(n))) %>% 
    group_by(price) %>% 
    mutate(prom_prop = mean(prop_price)) %>% 
    mutate(perfil = (prop_price / prom_prop - 1) %>% round(2))
  tabla
}


tabla <- tea %>% 
  count(how, price) %>% 
  group_by(how) %>% 
  mutate(prop_price = (100 * n / sum(n))) %>% 
  group_by(price) %>% 
  mutate(prom_prop = mean(prop_price)) %>% 
  mutate(perfil = (prop_price / prom_prop - 1) %>% round(2)) %>% 
  ungroup()
# Repeticiones

perfiles_rep <- rerun(100, perfiles_boot(tea)) %>% bind_rows(.id = 'muestra')## %>% map_dfr(~.x)


# Error estandard

perfiles_se <- perfiles_rep %>% 
  group_by(how, price) %>% 
  summarise(se = sd(perfil))



# Intervalos

perfiles_int <- tabla %>% 
  left_join(perfiles_se) %>% 
  mutate(Int_inf = perfil+qnorm(0.025)*se, Int_sup = perfil+qnorm(0.975)*se)
  

perfiles_int %>% 
ggplot() +
  geom_segment(aes(y = price, yend = price, x = Int_inf, xend = Int_sup), size = 1) +
  geom_point(aes(x = perfil, y = price), size = 2) +
  facet_wrap(how~.) +
  labs(x = 'perfil',
       y = 'precio') +
  theme_light()


# Ejercicio 2 -------------------------------------------------------------

library(estcomp)
# universo
enlace <- enlacep_2013 %>% 
  janitor::clean_names() %>% 
  mutate(id = 1:n()) %>% 
  select(id, cve_ent, turno, tipo, esp_3 = punt_esp_3, esp_6 = punt_esp_6, 
         n_eval_3 = alum_eval_3, n_eval_6 = alum_eval_6) %>% 
  na.omit() %>% 
  filter(esp_3 > 0, esp_6 > 0, n_eval_3 > 0, n_eval_6 > 0, cve_ent == "15")
set.seed(16021)
n <- 300
# muestra
enlace_muestra <- sample_n(enlace, n) %>% 
  mutate(clase = "muestra")

median(enlace$esp_3)

# 1. Crea un intervalo del 90% para $\hat{\theta}$ usando los percentiles de la distribución bootstrap, y $B=100$ replicaciones.

enlace_boot <- function(x,col){
  col <- enquo(col)
  n <- nrow(x)
  muestra <- sample_n(x,n, replace = TRUE)
  muestra %>% 
    select(!!col) %>% 
    unlist() %>% 
    median()
}


enlace_rep <- rerun(100, enlace_boot(enlace,esp_3)) %>% flatten_dbl()

quantile(enlace_rep, c(0.05, 0.95))


# 2. Podemos estimar el error estándar de Monte Carlo de los extremos de los intervalos (percentiles 0.05 y 0.95) haciendo bootstrap de la distribución bootstrap:
# Selecciona muestras con reemplazo de tamaño $B$ de la distribución bootstrap
#   + Calcula los percentiles de interés (0.05 y 0.95)

# Construimos la función bootstrap de la distribución bootstrap

enlace_boot_boot <- function(x){
  n <- length(x)
  muestra <- sample(x, size = n, replace = TRUE)
  tibble(Int_inf = quantile(muestra, c(0.025,0.975))[1], Int_sup = quantile(muestra, c(0.025,0.975))[2])
}

#Obtenemos las repeticiones
enlace_boot_rep <- rerun(1000,enlace_boot_boot(enlace_rep)) %>% map_dfr(~.x)

# + Calcula la desviación estándar de los percentiles (una para cada extremo), esta será tu aproximación al error de Monte Carlo

enlace_boot_sd <- map_dbl(enlace_boot_rep, sd)


# Ejercicio 3 -------------------------------------------------------------

   
# i) Genera una muestra aleatoria de tamaño $n=60$ con distribución $Poisson(\lambda)$, parámetro $\lambda=2.5$

# ii) Genera $10,000$ muestras bootstrap y calcula intervalos de confianza del 95\% para $\hat{\theta}$ usando 
# 1) el método normal, 2) percentiles y 3) $BC_a$.

poiss_boot <- function(x, ind){
  exp(-2*mean(x[ind]))
}


poiss_intervalos <- function(n=60) {
  poiss_muestra <- rpois(n,2.5)
  
  poiss_rep <-  boot(poiss_muestra, poiss_boot,10000)
  poiss_int <-boot.ci(poiss_rep, type = c("norm", "perc", "bca"))
  
  data.frame(metodo = c('normal','percentil','BCa'),
             theta = poiss_int$t0,
             inferior = c(poiss_int$normal[2],poiss_int$percent[4],poiss_int$bca[4]),
             superior = c(poiss_int$normal[3],poiss_int$percent[5],poiss_int$bca[5]))
}



# a) Repite el proceso descrito 1000 veces y llena la siguiente tabla:

poiss_rep_int <- rerun(1000, poiss_intervalos()) %>% bind_rows(.id = 'muestra')


poiss_rep_int <- poiss_rep_int %>% 
  mutate(fallo_izquierda = exp(-2*2.5)<inferior,
         fallo_derecha = exp(-2*2.5)>superior,
         Longitud = superior-inferior)

poiss_rep_int_res <- poiss_rep_int %>% 
  group_by(metodo) %>% 
  summarise(P_fallo_izquierda = sum(fallo_izquierda)/n(),
            P_fallo_derecha = sum(fallo_derecha)/n(),
            Cobertura = 1 - P_fallo_izquierda - P_fallo_derecha,
            Longitud_promedio = mean(Longitud))


# b) Realiza una gráfica de páneles, en cada panel mostrarás los resultados de uno de los métodos (normal, percentiles y BC_a),
# en el vertical graficarás los límites de los intervalos.
  ggplot(poiss_rep_int) +
    geom_pointrange(aes(x = reorder(muestra,theta),
                        ymin = inferior,
                        y=theta,
                        ymax = superior),
                    size=0.2) +
    geom_hline(yintercept = exp(-2.5*2)) +
    facet_grid(metodo~.)
# 
# c) Repite los incisos a) y b) seleccionando muestras de tamaño $300$.
  

# Ejercicio 5 --------------------------------------------------------------


  
# b) Utiliza el procedimiento descrito para generar observaciones de una variable aleatoria
#  con distribución geométrica y la relación entre la geométrica y la binomial negativa
#  para generar simulaciones de una variable aleatoria con distribución binomial negativa
# (parámetro p = 0.7, r = 20). Utiliza la semilla 341285 y reporta las primeras 10 simulaciones obtenidas.

  
#Para el caso de la geometrica tenemos:
    
set.seed(341285)
sim_geometrica <- function(p, n=1){
  U <- runif(n)
  q <- (1-p)
  as.integer(log(U)/log(q))+1
}
  
sim_geometrica(0.7,10)  
    
# Para el caso de la binomial negativa tenemos:

sim_binn <- function(p,r){
  sum(sim_geometrica(p,n=r))
}
 
rerun(10,sim_binn(0.7,20)) %>% flatten_dbl()
 
# c) Verifica la relación $$p_{j+1}=\frac{j(1-p)}{j+1-r}p_j$$ y úsala para generar
# un nuevo algoritmo de simulación, vuelve a definir la
# semilla y reporta las primeras 10 simulaciones.
set.seed(341285)
sim_binn_rec <- function(p,r){
  U <- runif(1)
  i <- r
  f <- p^r
  P <- f
  while(U>=P){
    f <- i*(1-p)*f/(i+1-r)
    P <- P+f
    i <- i+1
  }
  i
}

rerun(10, sim_binn_rec(0.7,20)) %>% flatten_dbl()

# d) Realiza 10,000 simulaciones usando cada uno de los algoritmos y compara el 
# tiempo de ejecución (puedes usar la función `system.time()`, explicada en 
# la sección de [rendimiento en R](https://tereom.github.io/est-computacional-2019/rendimiento-en-r.html)).

system.time(rerun(10000, sim_binn(0.7,20)))
system.time(rerun(10000, sim_binn_rec(0.7,20)))

# e) Genera un histogrma para cada algoritmo (usa 1000 simulaciones) y comparalo 
# con la distribución construida usando la función de R _dnbinom_.

binn_alg1 <- rerun(10000, sim_binn(0.7,20)) %>% flatten_dbl()
binn_alg2 <- rerun(10000, sim_binn_rec(0.7,20))%>% flatten_dbl()
binn_R <- (rnbinom(10000, size = 20, p = 0.7)+20) %>% flatten_dbl()

binn_hist <- tibble(Alg1 = binn_alg1,
                        Alg2 = binn_alg2,
                        R = binn_R) %>% 
  pivot_longer(Alg1:R,names_to = 'modelo', values_to = 'valor')

ggplot(binn_hist) + 
  geom_histogram(aes(valor), breaks = 20:45) +
  facet_grid(modelo~.) +
  theme_light()
  

# Ejercicio 4 -------------------------------------------------------------
# 4. Cobertura en la práctica
# En el caso del conteo rápido es posible evaluar la cobertura del intervalo de
# confianza bootstrap (tarea 5) usando los resultados de elecciones pasadas,
# para ello usaremos los resultados de las elecciones 2006
# repetirás los siguientes dos pasos 100 veces:
#   
# 1.Selecciona una muestra estratificada de election_2006 usando los tamaños de
# muestra que indica la tabla strata_sample_2006
# (donde n era el tamaño de muestra por estrato y N es el número de casillas en el mismo).


library(estcomp)
library(tidyverse)

muestra_2006 <- function(df=election_2006){
  df %>% 
    select(stratum,pri_pvem,pan,panal,prd_pt_conv,psd,otros) %>% 
    left_join(strata_sample_2006) %>%
    split(.$stratum) %>% 
    map_df(~sample_n(., size=first(.$n)))
}


estimador_razon <- function(df){
  df %>%
    pivot_longer(pri_pvem:otros, names_to = 'partido', values_to = 'votos') %>% 
    mutate(v_total = N*votos/n, total = sum(v_total)) %>% 
    group_by(partido) %>% 
    summarise(p=sum(v_total)/mean(total)) %>% 
    pivot_wider(names_from = partido, values_from = p)
}

elecciones_boot <- function(df=muestra){
  
  muestra_boot <- df %>% 
    group_by(stratum) %>%
    sample_frac(size = 1, replace = TRUE) %>% 
    ungroup()

    estimador_razon(muestra_boot)
}



elecciones_muestras_boot <- rerun(1000,elecciones_boot(muestra_2006)) %>% bind_rows()
elecciones_pi <- estimador_razon(muestra_2006)
elecciones_es <- map_dbl(elecciones_muestras_boot,sd)

elecciones_int <- rbind(razon = elecciones_pi) %>% 
  rbind(inferior = elecciones_pi + elecciones_es*qnorm(0.025)) %>% 
  rbind(superior =elecciones_pi + elecciones_es*qnorm(0.975)) %>% 
  t()
                            
elecciones_boot_rep <- function(df=election_2006){
  muestra <- muestra_2006(df)
  razon_pi <- estimador_razon(muestra)
  muestras_boot <- rerun(1000,elecciones_boot(muestra)) %>% bind_rows()
  error <- map_dbl(muestras_boot,sd)
  
  rbind(razon=razon_pi) %>% 
    rbind(inferior = razon_pi+error*qnorm(0.025)) %>% 
    rbind(superior = razon_pi+error*qnorm(0.975)) %>% 
    t() %>% 
    as.data.frame() %>% 
    rownames_to_column('partido')
    
}

elecciones_repeticiones <- rerun(2, elecciones_boot_rep()) %>% 
  bind_rows(.id = 'repeticion')

elecciones_repeticiones <- elecciones_repeticiones %>% 
  mutate(fallo_izquierda = razon<inferior,
         fallo_derecha = superior<razon,
         longitud = superior-inferior) %>% 
  group_by(partido) %>% 
  summarise(fallo_izquierda = sum(fallo_izquierda)/n(),
            fallo_derecha = sum(fallo_derecha)/n(),
            cobertura = 1 - fallo_izquierda - fallo_derecha,
            longitud = mean(longitud))

