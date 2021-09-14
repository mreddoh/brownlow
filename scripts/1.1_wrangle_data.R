
# Load packages ----
library(tidyverse)
library(here)

# Wrangle data ----

### * Add match id ----
match_chains %>% 
  mutate(grp = paste0(date, homeTeam.teamName, awayTeam.teamName)) %>% 
  group_by(grp) %>% 
  mutate(matchID = cur_group_id()) %>% 
  ungroup() %>% 
  select(-grp) ->
  match_chains

## Add position on ground grouping (Back, Mid, Fwd) ----

#### * First check x-y coordinates ----
coordinate_test_data <- match_chains %>% 
  filter(season==2021 & roundNumber==1 & homeTeam.teamName=="Richmond") %>% 
  select(venue.name,venueWidth,venueLength,x,y)

#### * Pythag to determine inside 50 arc ----
match_chains %>% 
  filter(season==2021 & roundNumber==1 & homeTeam.teamName=="Richmond") %>% 
  mutate(a1 = venueLength - (x + venueLength / 2),
         b = y,
         distance.to.goal = sqrt(a1^2 + b^2),
         a2 = (x + venueLength / 2),
         distance.from.goal = sqrt(a2^2 + b^2),
         position = case_when(distance.to.goal <= 50 ~ "FWD",
                              distance.from.goal <= 50 ~ "BCK",
                              TRUE ~ "MID")) -> 
  position_test_data

#### Add to dataset ----
match_chains %>% 
  mutate(position = case_when(sqrt((venueLength - (x + venueLength / 2))^2 + y^2) <= 50 ~ "FWD",
                              sqrt((x + venueLength / 2)^2 + y^2) <= 50 ~ "BCK",
                              TRUE ~ "MID")) -> 
  match_chains

## Create summarised chain information ----
match_chains_summary <- match_chains %>% 
  select(matchID,
         season, 
         roundNumber, 
         homeTeam.teamName, 
         awayTeam.teamName, 
         homeTeamScore.totalScore,
         awayTeamScore.totalScore,
         chain_number, 
         displayOrder, 
         initialState, 
         finalState,
         description,
         disposal,
         position) %>% 
  group_by(matchID, season, roundNumber, 
           homeTeam.teamName, awayTeam.teamName, 
           homeTeamScore.totalScore, awayTeamScore.totalScore,
           chain_number) %>% 
  summarise(initialState = max(initialState),
            finalState = max(finalState),
            initialPosition = max(ifelse(displayOrder==min(displayOrder),position,"AAAA")),
            finalPosition = max(ifelse(displayOrder==max(displayOrder),position,"AAAA"))) %>% 
  ungroup()

#### * Get chain averages for weightings ----

#### * Average score by initial position and initial state ----
match_chains_summary %>% 
  group_by(initialState, initialPosition) %>% 
  summarise(averageScore = sum(case_when(finalState=="goal" ~ 6, 
                                         finalState %in% c("behind","rushed","rushedOpp") ~ 1, 
                                         TRUE ~ 0)) / n()) %>% 
  arrange(-averageScore)

#### * Just look at predicted score by position -----
predicted_score <- match_chains_summary %>% 
  group_by(initialPosition) %>% 
  summarise(averageScore = sum(case_when(finalState=="goal" ~ 6, 
                                         finalState %in% c("behind","rushed","rushedOpp") ~ 1, 
                                         TRUE ~ 0)) / n()) %>% 
  arrange(-averageScore)


# Check kickIn and MID, probably shouldn't happen?
match_chains_summary %>% filter(initialState=="kickIn" & initialPosition=="MID")
# Will have to check footage, but maybe 50m penalties?


#### Apply weightings to chains ----
match_chains_summary %>% 
  left_join(.,predicted_score,by=c("initialPosition")) %>% 
  rename(averageScore.initial= averageScore) %>% 
  left_join(.,predicted_score,by=c("finalPosition"="initialPosition")) %>% 
  rename(averageScore.final  = averageScore) %>% 
  mutate(predicted.score.delta = averageScore.final - averageScore.initial) %>% 
  mutate(predicted.score.delta = case_when(finalState=="goal" ~ 6,
                                           finalState=="behind" ~ 1,
                                           finalState=="endQuarter" ~ 0,
                                           TRUE ~ predicted.score.delta)) %>% 
  select(-c("averageScore.initial","averageScore.final")) -> 
  # update dataset
  match_chains_summary

save(match_chains_summary, file = here("output","match_chains_summary.RData"))

