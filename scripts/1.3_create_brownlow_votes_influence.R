
# Load packages ----
library(tidyverse)
library(here)


## Apply votes using dplyr method ----
brownlow_votes <- attacking_influence_score %>% 
  group_by(matchID) %>% 
  mutate(votes = rank(-value, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  select(matchID,name,votes)


## Check Brownlow Medal Tally ----
brownlow_votes %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  arrange(-medal_tally) %>% 
  head(20)
