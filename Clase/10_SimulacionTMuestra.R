# Bernoulli bootstrap parametrico

simula_b <- function() {
  x <- rbernoulli(30, p = 0.3)
  sum(x)/30
}

rerun(1000, simula_b()) %>% flatten_dbl() %>% sd()


