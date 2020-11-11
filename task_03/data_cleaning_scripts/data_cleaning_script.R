library(tidyverse)
library(assertr)
library(readxl)

ship_data_record <- read_excel("~/dirty_data_project/task_03/data/seabirds.xls", sheet = 1)


clean_ship <- ship_data_record %>%
  select(-c(RECORD, SACT:LONGECELL)
  ) %>%
  rename(
    record_id = "RECORD ID",
    date = "DATE",
    time = "TIME",
    latitude = "LAT",
    longitude = "LONG",
    east_or_west_hemisphere = "EW",
  ) %>%
  mutate(
    time = str_remove(time, "1899-12-31"),
    time = str_remove(time, "1900-01-01"),
    east_or_west_hemisphere = str_replace(east_or_west_hemisphere, "E", "Eastern Hemisphere"),
    east_or_west_hemisphere = str_replace(east_or_west_hemisphere, "W", "Western Hemisphere")
  ) %>%
  drop_na(
)

write_csv(clean_ship, "clean_data//clean_ship.csv")

  