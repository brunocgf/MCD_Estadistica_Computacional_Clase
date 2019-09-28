library(estcomp)
library(tidyverse)

str(strata_sample_2006)
str(sample_2006)

sample <- sample_2006 %>% 
  left_join(strata_sample_2006) %>% 
  mutate(w = N/n)


f_razon <- function(df=sample, part){
  
  part <- enquo(part)
  
  num <- df %>%
    mutate(v = w*!!part) %>% 
    summarize(sum(v)) %>% 
    as.double()  
  
  den <- df %>%
    mutate(v = w*total) %>% 
    summarize(sum(v)) %>% 
    as.double()

  num/den
  
}

pri_pvem <- f_razon(sample, pri_pvem)
pan <- f_razon(sample, pan)
panal <- f_razon(sample, panal)
prd_pt_conv <- f_razon(sample, prd_pt_conv)
psd <- f_razon(sample, psd)


razon <- round(c(pri_pvem,pan,panal,prd_pt_conv,psd),4)
names(razon) <- c('pri_pvem','pan','panal','prd_pt_conv','psd')

x <- sample %>% 
  group_by(stratum) %>%
#  sample_n(size = 7200, replace = TRUE)
  sample_frac(size = 1, replace = TRUE) %>% 
  ungroup()

f_boot <- function(df=sample, part){
  
  part <- enquo(part)
  
  muestra <- df %>% 
    group_by(stratum) %>%
    sample_frac(size = 1, replace = TRUE) %>% 
    ungroup()
  
  f_razon(muestra, !!part)
  
}

b_pri_pvem <- rerun(1000, f_boot(sample, pri_pvem)) %>% flatten_dbl()

se_pri_pvem <- sd(b_pri_pvem)

int_pri_pvem <- c(pri_pvem+se_pri_pvem*qnorm(0.025), pri_pvem+se_pri_pvem*qnorm(0.975))






mb_pan <- rerun(1000, f_boot(sample, pan)) %>% flatten_dbl()
mb_panal <- rerun(1000, f_boot(sample, panal)) %>% flatten_dbl()
mb_prd_pt_conv <- rerun(1000, f_boot(sample, prd_pt_conv)) %>% flatten_dbl()
mb_pri_pvem <- rerun(1000, f_boot(sample, pri_pvem)) %>% flatten_dbl()
mb_psd <- rerun(1000, f_boot(sample, psd)) %>% flatten_dbl()
mb_otros <- rerun(1000, f_boot(sample, otros)) %>% flatten_dbl()

mb <- tibble(
  pan = mb_pan,
  panal = mb_panal,
  prd_pt_conv = mb_prd_pt_conv,
  pri_pvem = mb_pri_pvem,
  psd = mb_psd,
  otros = mb_otros,
)

b_se <- map_dbl(mb, sd)

data.frame(int_pan,
  int_panal,
  int_prd_pt_conv,
  int_prd_pt_conv,
  int_pri_pvem,
  int_psd,
  int_otros,
  row.names = c('Inf','Sup')) %>% 
  t(.) %>%
  round(4)

mb %>% 
  pivot_longer(cols = c('pan', 'prd_pt_conv', 'pri_pvem'), names_to = 'Partido', values_to = 'razon') %>% 
  ggplot() +
  geom_violin(aes(x = Partido, y = razon, fill = Partido), color = NA) +
  scale_fill_manual(values = c('blue','yellow','red')) +
  ggtitle('Intervalos Bootstrap del 95%') +
  theme_light()




