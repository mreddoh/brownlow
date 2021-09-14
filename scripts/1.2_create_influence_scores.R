
# Load packages ----
library(tidyverse)
library(here)
library(igraph)

## Create network graphs ----

#### * Create edgelist ----
edgelist <- match_chains %>% 
  filter(roundNumber <= 23) %>% #remove finals
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
  left_join(.,(match_chains_summary %>% select(matchID,chain_number,predicted.score.delta)),by=c("matchID","chain_number")) %>% 
  select(-c(displayOrder,team.teamName.from))


#### ** Test on one match and team ----

edgelist.matrix <- as.matrix(edgelist %>% 
                               filter(matchID==84) %>% 
                               filter(team=="Carlton") %>% 
                               filter(chain_number <= (match_chains %>% filter(matchID==84 & period<=2) %>% summarise(max(chain_number)) %>% pull())) %>% 
                               select(player.from, player.to)
)

graph <- graph.edgelist(edgelist.matrix, directed=TRUE)

plot(graph)

weights <- edgelist %>% 
  filter(matchID==1) %>% 
  filter(team=="Carlton") %>% 
  pull(predicted.score.delta)

score = match_chains %>% 
  filter(matchID==1) %>% 
  mutate(score = ifelse(homeTeam.teamName=="Carlton",homeTeamScore.totalScore,awayTeamScore.totalScore)) %>%
  select(score) %>% 
  unique() %>% 
  pull()

attacking_influence_score <- page_rank(graph, 
                                       #weights = weights, 
                                       weights = NULL, #hmmm... weights not acting as thought, will have to look further but first just try unweighted
                                       directed = TRUE)[[1]] %>% 
  enframe() %>% 
  mutate(value = value*score)


## Create scores by iterating through each match and team... ----

n = n_distinct(edgelist$matchID)*2

for (i in 1:n) {
  
  if (exists("attacking_influence_score") & i == 1) {rm(attacking_influence_score)}
  
  matchID_i = ceiling(i/2)
  team_i = edgelist %>% filter(matchID==matchID_i) %>% summarise(team = ifelse(i %% 2 == 1,max(team),min(team))) %>% pull()
  
  edgelist.matrix <- as.matrix(edgelist %>% 
                                 filter(matchID==matchID_i) %>% 
                                 filter(team==team_i) %>% 
                                 select(player.from, player.to))
  
  graph <- graph.edgelist(edgelist.matrix, directed=TRUE)
  
  weights <- edgelist %>% 
    filter(matchID==matchID_i) %>% 
    filter(team==team_i) %>% 
    pull(predicted.score.delta)
  
  score = match_chains %>% 
    filter(matchID==matchID_i) %>% 
    mutate(score = ifelse(homeTeam.teamName==team_i,homeTeamScore.totalScore,awayTeamScore.totalScore)) %>%
    select(score) %>% 
    unique() %>% 
    pull()
  
  temp <- page_rank(graph, weights = NULL, directed = TRUE)[[1]] %>% 
    enframe() %>% 
    mutate(value = value*score,
           matchID = matchID_i,
           team = team_i) %>% 
    select(matchID, team, name, value)
  
  if (i==1) {attacking_influence_score <- temp} else {attacking_influence_score <- rbind(attacking_influence_score,temp)}

}

save(attacking_influence_score, file = here("output","attacking_influence_score.RData"))

