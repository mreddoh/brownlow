
# Load packages ----
library(tidyverse)
library(here)
library(kableExtra)

## Apply votes using dplyr method ----
brownlow_votes <- attacking_influence_score %>% 
  group_by(matchID) %>% 
  mutate(votes = rank(-value, ties.method = "random")) %>% 
  filter(votes<=3) %>% 
  mutate(votes = rank(-votes, ties.method = "random")) %>% 
  ungroup() %>% 
  left_join(.,(match_chains_summary %>% select(matchID,roundNumber) %>% unique()),by=c("matchID")) %>% 
  rename(match_round = roundNumber) %>% 
  select(matchID,name,match_round,votes)

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
  arrange(-Total) %>% 
  knitr::kable(caption = "Brownlow Medal 2021 | Attacking Influence w/ pageRank Prediction") %>%
  kable_styling("striped", font_size = 14, position = "center", full_width = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE, color = "black", extra_css = "text-align:right") %>%
  column_spec(2:24, extra_css = "text-align:center") %>%
  kableExtra::save_kable(., file = here("predictions","influence_v1.0.html"))


