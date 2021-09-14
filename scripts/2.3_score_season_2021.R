
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
version = list.files(here("output")) %>% 
  tibble() %>% 
  filter(substr(.,1,1)=="v") %>% 
  mutate(value = as.numeric(substr(.,2,5))) %>% 
  summarise(max = paste0("v",max(value) + 0.01,"_model_gbm.RData")) %>% 
  pull(max)

load(file = here("output",version)) #model_obj
load(file = here("data","player_data_2021.Rdata"))

# Apply to 2021 season data ----

## * Prep data ----
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
player_data_2021.cleaned <- cbind(player_data_2021,team_portions,match_portions) %>% 
  mutate(team_result = ifelse(match_winner==player_team, match_margin, -1*match_margin))

ds_in <- player_data_2021.cleaned %>% 
  mutate(id = row_number()) %>% 
  select(id, all_of(model_vars), brownlow_votes)

## * Apply model ----
oot_processed <- bake(preprocessing_recipe, new_data = ds_in)

oot_prediction <- predict(object = final_model_fit, new_data = oot_processed) %>%
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
  select(match_id,season,match_round,name,votes)

## * Check Brownlow Medal Tally ----
result <- brownlow_votes %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  arrange(-medal_tally)

top20 <- brownlow_votes %>% 
  group_by(name) %>% 
  summarise(medal_tally = sum(votes)) %>% 
  arrange(-medal_tally) %>% 
  head(20) %>% 
  pull(name)

# Output graph like Channel 7 ----
brownlow_votes %>% 
  select(name,match_round,votes) %>% 
  rbind(.,(result %>% transmute(name = name, match_round = "99", votes = medal_tally))) %>% 
  filter(name %in% top20) %>% 
  arrange(as.integer(match_round)) %>% 
  pivot_wider(names_from = match_round, names_prefix = "R", values_from = votes, values_fill = 0) %>% 
  rename(Total = R99) %>% 
  arrange(-Total)

brownlow_votes %>% 
  select(name,match_round,votes) %>% 
  rbind(.,(result %>% transmute(name = name, match_round = "99", votes = medal_tally))) %>% 
  filter(name %in% top20) %>%
  arrange(as.integer(match_round)) %>% 
  pivot_wider(names_from = match_round, names_prefix = "R", values_from = votes, values_fill = 0) %>% 
  rename(Total = R99) %>% 
  arrange(-Total) %>% 
  knitr::kable(caption = "Brownlow Medal 2021 | Classic XGBoost Prediction") %>%
  kable_styling("striped", font_size = 14, position = "center", full_width = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE, color = "black", extra_css = "text-align:right") %>%
  column_spec(2:24, extra_css = "text-align:center") %>%
  kableExtra::save_kable(., file = here("predictions","classic_xgb_v1.4.html"))




