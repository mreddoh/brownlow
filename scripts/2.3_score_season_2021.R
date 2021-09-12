
# Load packages ----
library(tidyverse)
library(here)
library(tidymodels)
library(doParallel)
library(xgboost)
library(vip)

# speed up computation with parallel processing
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

# Load data ----
load(file = here("output","v1.00_model_gbm.RData")) #model_obj
load(file = here("data","player_data_2021.Rdata"))

# Apply to 2021 season data ----

## * Prep data ----
player_data_2021 %>% mutate(supercoach_score = afl_fantasy_score) -> player_data_2021

team_totals <- player_data_2021 %>% 
  group_by(match_id, player_team) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1:2],paste0('team.', names(.)[3:ncol(.)])))

match_totals <- player_data_2021 %>% 
  group_by(match_id) %>% 
  summarise_at(.vars = names(.)[27:78], sum) %>% 
  setNames(c(names(.)[1],paste0('match.', names(.)[2:ncol(.)])))

player_data_2021 %>% 
  left_join(.,team_totals,by=c("match_id","player_team")) %>% 
  left_join(.,match_totals,by=c("match_id")) ->
  player_data_2021

team_portions <- player_data_2021[27:78] / player_data_2021[,substr(names(player_data_2021),1,5)=="team."]
match_portions <- player_data_2021[27:78] / player_data_2021[,substr(names(player_data_2021),1,6)=="match."]

# assign new variable names
team_portions %>% setNames(object = ., nm = paste0('team_pct.', names(.)[1:ncol(.)])) -> team_portions
match_portions %>% setNames(object = ., nm = paste0('match_pct.', names(.)[1:ncol(.)])) -> match_portions

# combine
player_data_2021.cleaned <- cbind(player_data_2021,team_portions,match_portions)

ds_in <- player_data_2021.cleaned %>% 
  mutate(id = row_number()) %>% 
  select(id, player_position, team_pct.kicks:match_pct.spoils, brownlow_votes) %>% 
  select(-c(team_pct.brownlow_votes,match_pct.brownlow_votes))

## * Apply model ----
oot_processed <- bake(preprocessing_recipe, new_data = ds_in)

oot_prediction <- xgboost_model_final %>%
  fit(formula = brownlow_votes ~ ., data = oot_processed) %>%
  predict(new_data = oot_processed) %>%
  bind_cols(ds_in)

oot_out <- oot_prediction %>%
  select(id,.pred) %>%
  rename(predicted_votes_raw = .pred) %>% 
  left_join(.,(player_data_2021.cleaned %>% mutate(id = row_number())),by=c("id")) %>% 
  select(id:player_position,predicted_votes_raw) %>% 
  mutate(name = paste(player_first_name,player_last_name,sep=" "),
         season = substr(match_date,1,4))


# Calculate Votes ----

## * Apply votes using dplyr method ----
brownlow_votes <- oot_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  select(match_id,season,name,votes)

## * Check Brownlow Medal Tally ----
brownlow_votes %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  arrange(-medal_tally)


