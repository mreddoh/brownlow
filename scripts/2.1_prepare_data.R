
# Load packages ----
library(tidyverse)
library(here)

# Load data ----
load(file = here("data","player_data_2021.Rdata"))
load(file = here("data","player_data_full.Rdata"))
load(file = here("data","player_data_partial.Rdata"))

# Wrangle data into model-able dataset with normalised and cleaned variables ----
    # Note. look at normalised values, for example, disposals as percentage of teams disposals...

team_totals <- player_data_full %>% 
  group_by(match_id, player_team) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1:2],paste0('team.', names(.)[3:ncol(.)])))

match_totals <- player_data_full %>% 
  group_by(match_id) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1],paste0('match.', names(.)[2:ncol(.)])))


player_data_full %>% 
  left_join(.,team_totals,by=c("match_id","player_team")) %>% 
  left_join(.,match_totals,by=c("match_id")) ->
    player_data_full




