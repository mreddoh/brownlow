
# Load packages ----
library(tidyverse)
library(here)

# Get a list of files to import ----
list_of_files <- list.files(here("data"))

# Import chain data using file list ----
for (i in 1:length(list_of_files)) {
  
  if (exists("match_chains") & i == 1) {rm(match_chains)}
  
  temp <- read_csv(here("data",list_of_files[i]))
  
  if (i==1) {match_chains <- temp} else {match_chains <- rbind(match_chains,temp)}
  
  rm(temp)
  
}

