
# Load packages ----
library(tidyverse)
library(here)

# Load data ----
load(file = here("data","player_data_full.cleaned.Rdata"))


nat_fyfe.2019 <- player_data_full.cleaned %>% 
  filter(substr(match_date,1,4)=="2019") %>% 
  filter(player_last_name=="Fyfe")

fremantle_2019_votes <- full_out %>% 
  group_by(match_id) %>% 
  mutate(votes = rank(-predicted_votes_raw, ties.method = "random"),
         freo_game = max(ifelse(player_team=="Fremantle" & substr(match_date,1,4)=="2019",1,0))) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  filter(freo_game==1 & name %in% (full_out %>% filter(player_team=="Fremantle") %>% select(name) %>% unique() %>% pull())) %>% 
  select(match_id,season,name,votes) %>% 
  pivot_wider(names_from = match_id, values_from = votes, values_fill = 0)

# 19 votes





