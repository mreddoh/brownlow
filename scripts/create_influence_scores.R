
# Load packages ----
library(tidyverse)
library(here)
library(igraph)

## Create network graphs ----

#### * Create edgelist ----
edgelist <- match_chains %>% 
  filter(!is.na(disposal)) %>% 
  mutate(Player = paste(playerName.givenName,playerName.surname,sep=" ")) %>% 
  select(matchID, chain_number, displayOrder, Player, team.teamName) %>% 
  rename(player.to = Player) %>% 
  mutate(player.from = lag(player.to),
         team.teamName.from = lag(team.teamName)) %>% 
  group_by(matchID, chain_number) %>% 
  mutate(player.from = ifelse(displayOrder==min(displayOrder),NA,player.from)) %>% 
  ungroup() %>% 
  filter(!is.na(player.from)) %>% 
  filter(team.teamName.from == team.teamName) %>% 
  relocate(player.to, .after = player.from) %>% 
  rename(team = team.teamName) %>% 
  select(-c(displayOrder,team.teamName.from))


#### ** Test on one match and team ----

edgelist.matrix <- as.matrix(edgelist %>% 
                               filter(matchID==1) %>% 
                               filter(team=="Carlton") %>% 
                               select(player.from, player.to)
)

graph <- graph.edgelist(edgelist.matrix, directed=TRUE)

plot(graph)

attacking_influence_score <- page_rank(graph, weights = NULL, directed = TRUE)[[1]] %>% enframe()




names(attacking_influence_score)
