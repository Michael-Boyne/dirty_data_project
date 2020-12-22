# Decathlon script ------------------------------------------------------------ 

# library loadout
library(tidyverse)
library(janitor)

# load the data with clean col names
decathlon <- read_rds("data/decathlon.rds") %>%
  clean_names()

# jobs to clean the dataset
# 1. changing rownames column to an explicit column
# 2. rename some of the columns to be more readable
# 3. change the shape of the dataset into a long format

decathlon_rework <- decathlon %>%
  rownames_to_column("surname") %>%
  rename(
    one_hundred_meters = x100m,
    four_hundred_meters = x400m,
    hurdles_one_hundred_meters = x110m_hurdle,
    fifteen_hundred_meters = x1500m,
  ) %>%
  mutate(
    surname = tolower(surname),
    competition = as.character(competition)
  ) %>%
  pivot_longer(one_hundred_meters:fifteen_hundred_meters, names_to = "event", values_to = "score")

write_csv(decathlon_rework, "clean_data/clean_decathlon_ds.csv")


