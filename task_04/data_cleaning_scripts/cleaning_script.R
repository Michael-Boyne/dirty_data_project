library(tidyverse)
library(assertr)
library(tidyr)
library(readxl)
library(janitor)
library(car)

boing_boing_candy_fifteen <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
boing_boing_candy_sixteen <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
boing_boing_candy_seventeen <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

# Cleaning columns before join

clean_col_fifteen <- tibble::as.tibble(boing_boing_candy_fifteen, .name_repair = janitor::make_clean_names)
clean_col_sixteen <- tibble::as.tibble(boing_boing_candy_sixteen, .name_repair = janitor::make_clean_names)
clean_col_seventeen <- tibble::as.tibble(boing_boing_candy_seventeen, .name_repair = janitor::make_clean_names)

# 2015

clean_col_fifteen$year <- "2015"

clean_col_fifteen <- clean_col_fifteen %>%
  rename(
    age = "how_old_are_you",
    going_trick_or_treating = "are_you_going_actually_going_trick_or_treating_yourself"
  ) %>%
  pivot_longer(
    cols = c(butterfinger:york_peppermint_patties),
    names_to = "candy",
    values_to = "rating",
    values_drop_na = FALSE
  ) %>%
  drop_na(
    rating
  ) %>%
  select(
    year,
    age,
    candy,
    rating
  )

colnames(clean_col_fifteen)

# 2016

clean_col_sixteen$year <- "2016"

clean_col_sixteen <- clean_col_sixteen %>%
  rename(
    age = "how_old_are_you",
    going_trick_or_treating = "are_you_going_actually_going_trick_or_treating_yourself",
    gender = "your_gender",
    country = "which_country_do_you_live_in"
  ) %>%
  pivot_longer(
    cols = c(x100_grand_bar:york_peppermint_patties),
    names_to = "candy",
    values_to = "rating",
    values_drop_na = FALSE
  ) %>%
  drop_na(
    rating
  ) %>%
  select(
    year,
    age,
    gender,
    country,
    candy,
    rating
  )

# 2017

clean_col_seventeen$year <- "2017"

colnames(clean_col_seventeen) <- sub("q6_", "", colnames(clean_col_seventeen))

clean_col_seventeen <- clean_col_seventeen %>%
  rename(
    age = "q3_age",
    going_trick_or_treating = "q1_going_out",
    gender = "q2_gender",
    country = "q4_country"
) %>%
  pivot_longer(
    cols = c(`100_grand_bar`:york_peppermint_patties),
    names_to = "candy",
    values_to = "rating",
    values_drop_na = FALSE
  ) %>%
  drop_na(
    rating
  ) %>%
  select(
    year,
    age,
    gender,
    country,
    candy,
    rating
  )

# Full candy script

full_candy<- bind_rows(clean_col_fifteen,clean_col_sixteen, clean_col_seventeen)

clean_full_candy <- full_candy %>%
  mutate(
    candy = str_replace(candy, "x100_grand_bar", "100_grand_bar"),
    candy = str_replace(candy, "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes", "mary_janes"),
    candy = str_replace(candy, "anonymous_brown_globs_that_come_in_black_and_orange_wrappers", "mary_janes"),
    candy = str_replace(candy, "boxo_raisins", "box_o_raisins")
    ) %>%
  recode(clean_full_candy)


  
