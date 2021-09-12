
# Load packages ----
library(tidyverse)
library(here)
library(fitzRoy)


# Use fitzRoy to collect data ----

player_data_2021 <- fetch_player_stats(season = 2021, source = "fryzigg")

player_data_full <- fetch_player_stats(season = 2012:2020, source = "fryzigg")
player_data_partial <- fetch_player_stats(season = 1992:2011, source = "fryzigg")


save(player_data_2021, file = here("data","player_data_2021.Rdata"))
save(player_data_full, file = here("data","player_data_full.Rdata"))
save(player_data_partial, file = here("data","player_data_partial.Rdata"))




