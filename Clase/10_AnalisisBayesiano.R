library(tidyverse)
theta <- seq(0.05, 0.95, 0.1)
pesos.prior <- c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior <- pesos.prior/sum(pesos.prior) 
prior_df <- data_frame(theta, prior = round(prior, 3))


library(LearnBayes)

N <- 30 # estudiantes
z <- 11 # éxitos

# Verosimilitud
Like <- theta ^ z * (1 - theta) ^ (N - z)
product <- Like * prior

# Distribución posterior (normalizamos)
post <- product / sum(product)

dists <- bind_cols(prior_df, post = post)
round(dists, 3)


# También podemos usar la función pdisc
pdisc(p = theta, prior = prior, data = c(z, N - z)) %>% round(3)
#>  [1] 0.000 0.006 0.220 0.529 0.224 0.021 0.000 0.000 0.000 0.000

# Alargamos los datos para graficar
dists_l <- dists %>% 
  gather(dist, val, prior:post) %>% 
  mutate(dist = factor(dist, levels = c("prior", "post")))

ggplot(dists_l, aes(x = theta, y = val)) +
  geom_bar(stat = "identity", fill = "darkgray") + 
  facet_wrap(~ dist) +
  labs(x = expression(theta), y = expression(p(theta))) 

# ¿Cómo se ve la distribución posterior si tomamos una muestra de tamaño  90
# donde observamos la misma proporción de éxitos. Realiza los cálculos y graficala
# como un panel adicional de la gráfica anterior.

N <- 90
z <- 33

Like <- theta ^ z * (1 - theta) ^ (N - z)
product <- Like * prior

# Distribución posterior (normalizamos)
post2 <- product / sum(product)

dists <- bind_cols(prior_df, post = post, post2 = post2)
round(dists, 3)


# Alargamos los datos para graficar
dists_l <- dists %>% 
  gather(dist, val, prior:post2) %>% 
  mutate(dist = factor(dist, levels = c("prior", "post","post2")))

ggplot(dists_l, aes(x = theta, y = val)) +
  geom_bar(stat = "identity", fill = "darkgray") + 
  facet_wrap(~ dist) +
  labs(x = expression(theta), y = expression(p(theta))) 
