library(datasauRus)
library(gganimate)
library(tidyverse)


ggplot(datasaurus_dozen, aes(x = x, y = y, frame = dataset)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(
    dataset,
    transition_length = 2,
    state_length = 2
  ) +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')


