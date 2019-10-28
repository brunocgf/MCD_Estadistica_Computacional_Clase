
# simulacion --------------------------------------------------------------


library(MASS)
library(tidyverse)


beauty <- readr::read_csv("https://raw.githubusercontent.com/tereom/est-computacional-2018/master/data/beauty.csv")
fit_score <- lm(courseevaluation ~ age + btystdave + female + nonenglish, data = beauty)


fun_simula_beta <- function(){
  
  df = summary(fit_score)$df[[2]]
  sigma <- summary(fit_score)$sigma*sqrt(df/rchisq(1,df))
  
  beta <- mvrnorm(1, mu = fit_score$coefficients,
                  Sigma = (sigma^2)*summary(fit_score)$cov.unscaled)
  
  list(sigma = sigma, beta =  beta)
  
}

simula_beta <- rerun(10000, fun_simula_beta())

fun_simula_calif <- function(x, beta, sigma){
  
  mu <- beta[1] + beta[2]*x[1] + beta[3]*x[2] + beta[4]*x[3] + beta[5]*x[4]
  obs <- rnorm(1,mu,sigma)
  
}

simula_puntajes_A <- map(simula_beta,
                       ~fun_simula_calif(x = c(50,-1,1,1), beta = .[['beta']], sigma = .[['sigma']])) %>%
  flatten_dbl()  

simula_puntajes_B <- map(simula_beta,
                         ~fun_simula_calif(x = c(60,-0.5,0,1), beta = .[['beta']], sigma = .[['sigma']])) %>%
  flatten_dbl()

simulacion <- tibble(A = simula_puntajes_A, B = simula_puntajes_B) %>% 
  mutate(Diferencia = A-B)

ggplot(simulacion) +
  geom_histogram(aes(A), color = 'blue', fill = NA) +
  geom_histogram(aes(B), color = 'red', fill = NA)

ggplot(simulacion) +
  geom_histogram(aes(Diferencia)) 

sum(simulacion$Diferencia>0)/10000

simula_beta3 <- rerun(6000, fun_simula_beta()) %>% 
  map_dbl(~.[['beta']][[3]]) %>% 
  as.data.frame()

names(simula_beta3) <- 'beta_3'


ggplot(simula_beta3) +
  geom_histogram(aes(beta_3))

m_b3 <- mean(simula_beta3$beta_3)
sd_b3 <- sd(simula_beta3$beta_3)

m_b3_sum <- summary(fit_score)$coeff[3,1]
sd_b3_sum <- summary(fit_score)$coeff[3,2]


# nullabor ----------------------------------------------------------------

library(nullabor)
library(ggplot2)

data(wasps) # incluídos en nullabor

wasp_lda <- MASS::lda(Group~., data = wasps[,-1])
wasp_ld <- predict(wasp_lda, dimen = 2)$x
true <- data.frame(wasp_ld, Group = wasps$Group)

ggplot(true, aes(x = LD1, y = LD2, colour = Group)) + 
  geom_point() + 
  theme(aspect.ratio = 1)

wasp_null <- lineup(
  null_permute('Group'),
  n = 19,
  true
)


ggplot(wasp_null, aes(x = LD1, y = LD2, colour = Group)) +
  geom_point() +
  facet_wrap(~.sample)
