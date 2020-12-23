# cake cleaning script -------------------------------------------------------

# library loadout
library(tidyverse) 
library(janitor)

# load the data with clean column names
dogs <- read_csv("data/dog_survey.csv") %>%
  clean_names()

# cleaning jobs for dogs
dogs %>%
  remove_empty("cols") %>%
  rename("title" = "Title") %>%
  