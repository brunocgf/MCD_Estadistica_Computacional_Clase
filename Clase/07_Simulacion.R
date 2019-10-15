mid_square <- function(seed, n) {
  seeds <- numeric(n)
  values <- numeric(n)
  for (i in 1:n) {
    x <- seed ^ 2
    seed = case_when(
      nchar(x) > 2 ~ (x %/% 1e2) %% 1e4,
      TRUE ~ 0)
    values[i] <- x
    seeds[i] <- seed
  }
  cbind(seeds, values)
}
x <- mid_square(1931, 10) 

x <- mid_square(9731, 100) 

# Utiliza la función runif de R y el método de inversión para generar 1000 simulaciones
# de una variable aleatoria $X$ tal que $p_1=0.20, p_2= 0.15, p_3=0.25, p_4=0.40$
# donde $p_j=P(X=j)$.

inv_ej <- function(n = 1000){
  sims_unif <- runif(n)
  sims_x <- case_when( 
    sims_unif < 0.4 ~ 4, 
    sims_unif < 0.65 ~ 3,
    sims_unif < 0.85 ~ 1,
    TRUE ~ 2
  )
}
    
  



# Simular gamma(3/2,1)

cociente <- function(x){
  3/2 * 1/base::gamma(3/2) * x ^(1/2)*exp(-x/3)
}

gamma <- function(){
  
  c <- 1.26
  x <- rexp(1,2/3)
  u <- runif(1)
  while(u>cociente(x)/c){

    u <- runif(1)
    x <- rexp(1,2/3)
  }
  x
}

cociente_normal <- function(x){
  exp(((x-1)^2)/2)
}

normal <- function(){
  
  y <- rexp(1,1)
  u <- runif(1)
  
  while(u>cociente_normal(y)){
    y <- rexp(1,1)
    u <- runif(1)
  }
  y*(-1)^round(runif(1))
}

n <- rerun(1000,normal()) %>% flatten_dbl()

n <- c(n,-n)
