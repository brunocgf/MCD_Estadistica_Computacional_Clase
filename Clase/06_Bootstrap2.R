library(bootstrap)
library(tidyverse)

# Comapara el intervalo anterior con los intervalos normal y de percentiles.

v <- sum((spatial$A - mean(spatial$A)) ^ 2) / nrow(spatial)

var_sesgada <- function(x) sum((x - mean(x)) ^ 2) / length(x)

var_muestra <- function(x){
  n <- length(x)
  muestra_boot <- sample(x, size = n, replace = TRUE)
  var_sesgada(muestra_boot)
}


m <- rerun(2000, var_muestra(spatial$A)) %>% flatten_dbl()

se <- sd(m)

#Intervalo normal
c(v+se*qnorm(0.025),v+ se*qnorm(0.975))

?#Intervalo quantil
quantile(m, c(0.025, 0.975))
