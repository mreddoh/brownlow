
# Load packages ----
library(tidyverse)
library(here)

# Load data ----
load(file = here("data","player_data_2021.Rdata"))
load(file = here("data","player_data_full.Rdata"))
load(file = here("data","player_data_partial.Rdata"))

# Wrangle data into model-able dataset with normalised and cleaned variables ----
    # Note. look at normalised values, for example, disposals as percentage of teams disposals...

## * Create team and match total variables ----
team_totals <- player_data_full %>% 
  group_by(match_id, player_team) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1:2],paste0('team.', names(.)[3:ncol(.)])))

match_totals <- player_data_full %>% 
  group_by(match_id) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1],paste0('match.', names(.)[2:ncol(.)])))

## * Join on variables ----
player_data_full %>% 
  left_join(.,team_totals,by=c("match_id","player_team")) %>% 
  left_join(.,match_totals,by=c("match_id")) ->
    player_data_full


team_portions <- player_data_full[27:78] / player_data_full[,substr(names(player_data_full),1,5)=="team."]
match_portions <- player_data_full[27:78] / player_data_full[,substr(names(player_data_full),1,6)=="match."]

## * Assign new variable names ----
team_portions %>% setNames(object = ., nm = paste0('team_pct.', names(.)[1:ncol(.)])) -> team_portions
match_portions %>% setNames(object = ., nm = paste0('match_pct.', names(.)[1:ncol(.)])) -> match_portions

## * Combine datasets ----
player_data_full.cleaned <- cbind(player_data_full,team_portions,match_portions)

# Add in new variables based on historical Brownlow performance ----




# Add in new variables based result, i.e. was player in winning team? ----






# Save data ----
save(player_data_full.cleaned, file = here("data","player_data_full.cleaned.Rdata"))
