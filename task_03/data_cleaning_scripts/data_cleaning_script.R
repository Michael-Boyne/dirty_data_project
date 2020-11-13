
# Package load-in

library(tidyverse)
library(assertr)
library(readxl)
library(janitor)
library(stringr)

# Reading in the two xls files

ship_data_record <- read_excel("~/dirty_data_project/task_03/data/seabirds.xls", sheet = 1)
bird_data_record <- read_excel("~/dirty_data_project/task_03/data/seabirds.xls", sheet = 2)

# I decided to clean the two scripts individually before joining.

# Cleaning the ship_data_record
# The only columns that were really necessary to answer the questions were the record_id (which
# is the primary key for this sheet and the foreign key in the bird_record), date, latitude,
# longitude, and east_or_west_hemisphere. 

clean_ship <- ship_data_record %>%
  select(-c(RECORD, TIME, SACT:LONGECELL)
  ) %>%
  rename(
    record_id = "RECORD ID",
    date = "DATE",
    latitude = "LAT",
    longitude = "LONG",
    east_or_west_hemisphere = "EW",
  ) %>%
  mutate(
    east_or_west_hemisphere = str_replace(east_or_west_hemisphere, "E", "Eastern Hemisphere"),
    east_or_west_hemisphere = str_replace(east_or_west_hemisphere, "W", "Western Hemisphere")
  ) %>%
  drop_na(
)

# Cleaning the bird_data_record
# Before doing any manual cleaning, it is good to make use of the janitor package that will clean
# much of what needs to be changed in the column names.
# A reason for using janitor for this sheet and not the previous one was some of the column names were
# particularly messy and needed fixed before manual interaction.

clean_birds_cols <- tibble::as_tibble(bird_data_record, .name_repair = janitor::make_clean_names)

recleaned_birds <- clean_birds %>%
  select(
   record,
   record_id,
   species_common_name_taxon_age_sex_plumage_phase,
   species_scientific_name_taxon_age_sex_plumage_phase,
   species_abbreviation,
   count
  ) %>%
  rename(
    species_common_name = "species_common_name_taxon_age_sex_plumage_phase",
    species_scientific_name = "species_scientific_name_taxon_age_sex_plumage_phase",
    bird_count = "count"
  ) %>%
  mutate(
    species_common_name = str_remove(species_common_name, "AD"),
    species_common_name = str_remove(species_common_name, "IMM"),
    species_common_name = str_remove(species_common_name, "SUBAD"),
    species_common_name = str_remove(species_common_name, "SUB"),
    species_common_name = str_remove(species_common_name, "JUV"),
    species_common_name = str_remove(species_common_name, "PL1"),
    species_common_name = str_remove(species_common_name, "PL2"),
    species_common_name = str_remove(species_common_name, "PL3"),
    species_common_name = str_remove(species_common_name, "PL4"),
    species_common_name = str_remove(species_common_name, "PL5"),
    species_scientific_name = str_remove(species_scientific_name, "AD"),
    species_scientific_name = str_remove(species_scientific_name, "IMM"),
    species_scientific_name = str_remove(species_scientific_name, "SUBAD"),
    species_scientific_name = str_remove(species_scientific_name, "SUB"),
    species_scientific_name = str_remove(species_scientific_name, "JUV"),
    species_scientific_name = str_remove(species_scientific_name, "PL1"),
    species_scientific_name = str_remove(species_scientific_name, "PL2"),
    species_scientific_name = str_remove(species_scientific_name, "PL3"),
    species_scientific_name = str_remove(species_scientific_name, "PL4"),
    species_scientific_name = str_remove(species_scientific_name, "PL5"), 
    species_abbreviation = str_remove(species_abbreviation, "AD"),
    species_abbreviation = str_remove(species_abbreviation, "IMM"),
    species_abbreviation = str_remove(species_abbreviation, "SUBAD"),
    species_abbreviation = str_remove(species_abbreviation, "SUB"),
    species_abbreviation = str_remove(species_abbreviation, "JUV"),
    species_abbreviation = str_remove(species_abbreviation, "PL1"),
    species_abbreviation = str_remove(species_abbreviation, "PL2"),
    species_abbreviation = str_remove(species_abbreviation, "PL3"),
    species_abbreviation = str_remove(species_abbreviation, "PL4"),
    species_abbreviation = str_remove(species_abbreviation, "PL5")
  ) %>%
  drop_na(bird_count)

# Using a left join on the two tables

clean_bird_ship <- left_join(recleaned_birds, clean_ship, "record_id")

# writing a csv to use in my analysis

write_csv(clean_bird_ship, "clean_data/clean_bird_ship.csv")