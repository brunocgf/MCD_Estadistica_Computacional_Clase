library(tidyverse)
library(estcomp)



data("election_sub_2012")

ggplot(data = election_sub_2012) +
  geom_point(mapping = aes(x = total, y = prd_pt_mc, color =  polling_type))

p <- ggplot(election_sub_2012, aes(x = pan_pct, y = prd_pt_mc_pct))
p + geom_point(size = 0.9)
p + geom_jitter(size = 0.9)

ggplot(election_sub_2012, aes(x = reorder(state_abbr, prd_pt_mc), y = prd_pt_mc_pct)) +
  geom_jitter(size = 0.8) +
  geom_boxplot(outlier.color = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Votos por casilla y estado", 
       subtitle = "PRD-PT-MC", x = "estado", y = "total de votos")

## Lee la ayuda de reorder y repite las gr?ficas anteriores ordenando por la mediana
## de hwy.

ggplot(election_sub_2012, aes(x = reorder(state_abbr, prd_pt_mc, median), 
                              y = prd_pt_mc)) +
  geom_jitter(alpha = 0.6, size = 0.8) +
  geom_boxplot(outlier.color = NA) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Votos por casilla y estado", 
       subtitle = "PRD-PT-MC", x = "estado", y = "total de votos")

## ?C?mo har?as para graficar los puntos encima de las cajas de boxplot?

ggplot(election_sub_2012, aes(x = reorder(state_abbr, prd_pt_mc, median), 
                              y = prd_pt_mc)) +
  geom_boxplot(outlier.color = NA) +
  geom_jitter(alpha = 0.6, size = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Votos por casilla y estado", 
       subtitle = "PRD-PT-MC", x = "estado", y = "total de votos")


ggplot(filter(election_sub_2012, !is.na(section_type)),
       aes(x = reorder(state_abbr, pri_pvem_pct, median), y = pri_pvem_pct)) + 
  geom_boxplot() +
  facet_wrap(~ section_type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


election_region_2012 <- election_2012 %>% 
  group_by(region, section_type) %>% 
  summarise_at(vars(pri_pvem:total), sum) %>% 
  mutate_at(vars(pri_pvem:otros), .funs = ~ 100 * ./total) %>% 
  ungroup() %>% 
  mutate(region = reorder(region, pri_pvem)) %>%
  gather(party, prop_votes, pri_pvem:otros) %>% 
  filter(!is.na(section_type))

ggplot(election_region_2012, aes(x = reorder(party, prop_votes), 
                                 y = prop_votes, fill = reorder(party, -prop_votes))) +
  geom_col(show.legend = FALSE) +
  facet_grid(region ~ section_type) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
