library(tidyverse)
library(gridExtra)

# Metropolis

# Introduccion ------------------------------------------------------------


islas <- data.frame(islas = 1:10, pob = 1:10)

caminaIsla <- function(i){ # i: isla actual
  u <- runif(1) # volado
  v <- ifelse(u < 0.5, i - 1, i + 1)  # isla vecina (índice)
  if(v < 1 | v > 10){ # si estas en los extremos y el volado indica salir
    return(i)
  }
  u2 <- runif(1)
  p_move = min(islas$pob[v] / islas$pob[i], 1)
  if(p_move  > u2){
    return(v) # isla destino
  }
  else{
    return(i) # me quedo en la misma isla
  }
}

pasos <- 1000
camino <- numeric(pasos)
camino[1] <- sample(1:10, 1) # isla inicial
for(j in 2:pasos){
  camino[j] <- caminaIsla(camino[j - 1])
}

caminata <- tibble(pasos = 1:pasos, isla = camino)

plot_caminata <- ggplot(caminata[1: 1000, ], aes(x = pasos, y = isla)) +
  geom_point(size = 0.8) +
  geom_path(alpha = 0.5) +
  coord_flip() + 
  labs(title = "Caminata aleatoria") +
  scale_y_continuous(expression(theta), breaks = 1:10) +
  scale_x_continuous("Tiempo")

plot_dist <- ggplot(caminata, aes(x = isla)) +
  geom_histogram() +
  scale_x_continuous(expression(theta), breaks = 1:10) +
  labs(title = "Distribución objetivo", 
       y = expression(P(theta)))

grid.arrange(plot_caminata, plot_dist, ncol = 1, heights = c(4, 2))


# Bernoulli ---------------------------------------------------------------


# p(theta) con theta = 0.4, a = 2, b = 2
dbeta(0.4, 2, 2)

# Definimos la distribución inicial
prior <- function(a = 1, b = 1){
  function(theta) dbeta(theta, a, b)
}

# Verosimilitid binomial
likeBern <- function(z, N){
  function(theta){
    theta ^ z * (1 - theta) ^ (N - z)
  }
}

# posterior no normalizada
postRelProb <- function(theta){
  mi_like(theta) * mi_prior(theta)
}


# Datos observados
N <-  14
z <- 11

# Defino mi inicial y la verosimilitud
mi_prior <- prior() # inicial uniforme
mi_like <- likeBern(z, N) # verosimilitud de los datos observados

# para cada paso decidimos el movimiento de acuerdo a la siguiente función

caminaAleat <- function(theta){ # theta: valor actual

  salto_prop <- rnorm(1, 0, sd = 0.1) # salto propuesto
  theta_prop <- theta + salto_prop # theta propuesta
  if(theta_prop < 0 | theta_prop > 1){ # si el salto implica salir del dominio
    return(theta)
  }
  u <- runif(1) 
  p_move <-  min(postRelProb(theta_prop) / postRelProb(theta), 1) # prob mover
  if(p_move  > u){
    return(theta_prop) # aceptar valor propuesto

  }
  else{
    return(theta) # rechazar

  }
}

set.seed(47405)

pasos <- 6000
camino <- numeric(pasos) # vector que guardará las simulaciones
camino[1] <- 0.1 # valor inicial

# Generamos la caminata aleatoria
aceptado <- 0
for (j in 2:pasos){
  camino[j] <- caminaAleat(camino[j - 1])
  aceptado <- (camino[j] != camino[j-1])+aceptado
}
aceptado/6000

caminata <- data.frame(pasos = 1:pasos, theta = camino)

ggplot(caminata[1:3000, ], aes(x = pasos, y = theta)) +
  geom_point(size = 0.8) +
  geom_path(alpha = 0.5) +
  scale_y_continuous(expression(theta), limits = c(0, 1)) +
  scale_x_continuous("Tiempo") +
  geom_vline(xintercept = 600, color = "red", alpha = 0.5)


# excluímos las primeras observaciones (etapa de calentamiento)
caminata_f <- filter(caminata, pasos > 600)
ggplot(caminata_f, aes(x = theta)) +
  geom_density(adjust = 2, aes(color = "posterior")) +
  labs(title = "Distribución posterior", 
       y = expression(p(theta)), 
       x = expression(theta)) + 
  stat_function(fun = mi_prior, aes(color = "inicial")) + # inicial
  xlim(0, 1)

mean(caminata_f$theta)
sd(caminata_f$theta)

sims_y <- rbinom(nrow(caminata_f), size = 1, prob = caminata_f$theta)
mean(sims_y) # p(y = 1 | x) probabilidad predictiva
sd(sims_y)


# Inferencia de dos proporciones binomiales -------------------------------

# Verosimilitid binomial
likeBern2 <- function(z_1, N_1, z_2, N_2){
  function(theta){
    theta[1] ^ z_1 * (1 - theta[1]) ^ (N_1 - z_1) *
      theta[2] ^ z_2 * (1 - theta[2]) ^ (N_2 - z_2)
  }
}

prior2 <- function(a_1 = 3, b_1 = 3, a_2 = 3, b_2 = 3){
  function(theta) dbeta(theta[1], a_1 , b_1) * dbeta(theta[2], a_2 , b_2)
}

# posterior no normalizada
postRelProb2 <- function(theta){
  mi_like(theta) * mi_prior(theta)
}

z_1=5; N_1=7; z_2=2; N_2=7; a_1=3; a_2=3; b_1=3; b_2=3
# Datos observados
N = 14
z = 11

# Defino mi inicial y la verosimilitud
mi_prior <- prior2() # inicial uniforme
mi_like <- likeBern2(z_1 = z_1, N_1 = N_1, z_2 = z_2, N_2 = N_2) # verosimilitud

# para cada paso decidimos el movimiento de acuerdo a la siguiente función
caminaAleat2 <- function(theta){ # theta: valor actual
  salto_prop <- MASS::mvrnorm(n = 1 , mu = rep(0, 2), 
                              Sigma = matrix(c(0.08, 0, 0, 0.08), byrow = TRUE, nrow = 2)) # salto propuesto
  theta_prop <- theta + salto_prop # theta propuesta
  if(all(theta_prop < 0) | all(theta_prop > 1)){ # salir del dominio
    return(theta)
  }
  u <- runif(1) 
  p_move = min(postRelProb2(theta_prop) / postRelProb2(theta), 1) # prob mover
  if(p_move  > u){
    return(theta_prop) # aceptar valor propuesto
  }
  else{
    return(theta) # rechazar
  }
}

set.seed(47405)

pasos <- 6000
camino <- matrix(0, nrow = pasos, ncol = 2) # vector que guardará las simulaciones
camino[1, ] <- c(0.1, 0.8) # valor inicial

# Generamos la caminata aleatoria
for (j in 2:pasos){
  camino[j, ] <- caminaAleat2(camino[j - 1, ])
}

caminata <- data.frame(pasos = 1:pasos, theta_1 = camino[, 1], 
                       theta_2 = camino[, 2])

ggplot(caminata, aes(x = theta_1, y = theta_2)) +
  geom_point(size = 0.8) +
  geom_path(alpha = 0.3) +
  scale_x_continuous(expression(theta[1]), limits = c(0, 1)) +
  scale_y_continuous(expression(theta[2]), limits = c(0, 1)) +
  coord_fixed()

caminata_m <- caminata %>%
  gather(parametro, val, theta_1, theta_2) %>%
  arrange(pasos)

ggplot(caminata_m[1:2000, ], aes (x = pasos, y = val)) +
  geom_path(alpha = 0.5) +
  facet_wrap(~parametro, ncol = 1) +
  scale_y_continuous("", limits = c(0, 1))
