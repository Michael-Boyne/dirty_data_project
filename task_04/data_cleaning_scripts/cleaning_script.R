
# Package load-out

library(tidyverse)
library(assertr)
library(tidyr)
library(readxl)
library(janitor)

# Reading in the three xls files

boing_boing_candy_fifteen <- read_xlsx("raw_data/boing-boing-candy-2015.xlsx")
boing_boing_candy_sixteen <- read_xlsx("raw_data/boing-boing-candy-2016.xlsx")
boing_boing_candy_seventeen <- read_xlsx("raw_data/boing-boing-candy-2017.xlsx")

# Cleaning columns before join

clean_col_fifteen <- tibble::as.tibble(boing_boing_candy_fifteen, .name_repair = janitor::make_clean_names)
clean_col_sixteen <- tibble::as.tibble(boing_boing_candy_sixteen, .name_repair = janitor::make_clean_names)
clean_col_seventeen <- tibble::as.tibble(boing_boing_candy_seventeen, .name_repair = janitor::make_clean_names)

# cleaning the data for 2015

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
    going_trick_or_treating,
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
    going_trick_or_treating,
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
    going_trick_or_treating,
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
    candy = str_replace(candy, "boxo_raisins", "box_o_raisins"),
    country = recode(country,
                     "us" = "USA",
                     "Us" = "USA",
                     "US" = "USA",
                     "'merica" = "USA",
                     "Alaska" = "USA",
                     "USAAa" = "USA",
                     "USAA" = "USA",
                     "United States of America" = "USA",
                     "uSA" = "USA",
                     "united states" = "USA",
                     "United States" = "USA",
                     "U.S.A." = "USA",
                     "Murica" = "USA",
                     "USAA!" = "USA",
                     "USAA (I think but it's an election year so who can really tell)" = "USA",
                     "U.S." = "USA",
                     "America" = "USA",
                     "Units States" = "USA",
                     "United states" = "USA",
                     "USAA USA USA" = "USA",
                     "the best one - USAAa" = "USA",
                     "USAA! USA! USA!" = "USA",
                     "u.s." = "USA",
                     "The Yoo Ess of Aaayyyyyy" = "USA",
                     "USA!" = "USA",
                     "USA (I think but it's an election year so who can really tell)" = "USA",
                     "USA USA USA" = "USA",
                     "the best one - USA" = "USA",
                     "USA! USA! USA!" = "USA",
                     "USA of america" = "USA",
                     "USA!!!!!!" = "USA",
                     "USA! USA!" = "USA",
                     "United Sates" = "USA",
                     "Sub-Canadian North USA... 'Merica" = "USA",
                     "Trumpistan" = "USA",
                     "U.s." = "USA",
                     "Merica" = "USA",
                     "UNited States" = "USA",
                     "United Stetes" = "USA",
                     "america" = "USA",
                     "USA USA USA USA" = "USA",
                     "United  States of USA" = "USA",
                     "United State" = "USA",
                     "United staes" = "USA",
                     "USAa." = "USA",
                     "USAUSAUSA" = "USA",
                     "USA of A" = "USA",
                     "Unites States" = "USA",
                     "The USA" = "USA",
                     "North Carolina" = "USA",
                     "U S" = "USA",
                     "USA? Hard to tell anymore.." = "USA",
                     "Pittsburgh" = "USA",
                     "New York" = "USA",
                     "California" = "USA",
                     "USAa" = "USA",
                     "Ahem....Amerca" = "USA",
                     "New Jersey" = "USA",
                     "United Stated" = "USA",
                     "United Statss" = "USA",
                     "murrika" = "USA",
                     "united States" = "USA",
                     "N. USA" = "USA",
                     "USASA" = "USA",
                     "United Statea" = "USA",
                     "USA USA USA!!!!" = "USA",
                     "USA (I think but it's an election year so who can really tell)" = "USA",
                     "USA USA" = "USA",
                     "USA!!" = "USA",
                     "USA? Hard to tell anymore.." = "USA",
                     "USA USA!" = "USA",
                     "USAd" = "USA",
                     "I pretend to be from Canada, but I am really from the USA." = "USA",
                     "USA? Hard to tell anymore.." = "USA",    
                     "canada" = "Canada",
                     "CANADA" = "Canada",
                     "USA!!!" = "USA",
                     "USA!" = "USA",
                     "USA (I think but it's an election year so who can really tell)" = "USA",
                     "Scotland" = "United Kingdom",
                     "endland" = "United Kingdom",
                     "USA (I think but it's an election year so who can really tell)" = "USA",
                     "uk" = "United Kingdom",
                     "england" = "United Kingdom",
                     "USA? Hard to tell anymore.." = "USA",
                     "spain" = "Spain",
                     "Uk" = "United Kingdom",
                     "Canada`" = "Canada",
                     "netherlands" = "The Netherlands",
                     "Netherlands" = "The Netherlands",
                     "kenya" = "Kenya",
                     "germany" = "Germany",
                     "South africa" = "South Africa",
                     "cascadia" = "USA",
                     "The republic of Cascadia" = "USA",
                     "sweden" = "Sweden",
                     "AUSAtria" = "Austrlia",
                     "AUSAtralia" = "Australia",
                     "españa" = "Spain",
                     "Cascadia" = "USA",
                     "England" = "United Kingdom",
                     "croatia" = "Croatia",
                     "belgium" = "Belgium",
                     "Korea" = "South Korea",
                     "france" = "France",
                     "UK" = "United Kingdom",
                     "hungary" = "Hungary"
                    ),
    country = na_if(country, "Fear and Loathing"),
    country = na_if(country, "sUSAribe to dm4uz3 on youtube"),
    country = na_if(country, "Narnia"),
    country = na_if(country, "Atlantis"),
    country = na_if(country, "Canae"),
    country = na_if(country, "Can"),
    country = na_if(country, "A"),
    country = na_if(country, "insanity lately"),
    country = na_if(country, "Earth"),
    country = na_if(country, "Europe"),
    country = na_if(country, "Denial"),
    country = na_if(country, "Not the USA or Canada"),
    country = na_if(country, "30.0"),
    country = na_if(country, "45.0"),
    country = na_if(country, "EUA"),
    country = na_if(country, "god's country"),
    country = na_if(country, "44.0"),
    country = na_if(country, "54.0"),
    country = na_if(country, "Somewhere"),
    country = na_if(country, "one of the best ones"),
    country = na_if(country, "there isn't one for old men"),
    country = na_if(country, "47.0"),
    country = na_if(country, "51.0"),
    country = na_if(country, "this one"),
    country = na_if(country, "Neverland"),
    country = na_if(country, "A tropical island south of the equator"),
    country = na_if(country, "I don't know anymore"),
    country = na_if(country, "See above")
    )

# Changing the age column from a character vector to a age vector, so that I can remove all entries that aren't valid ages

clean_full_candy$age <- as.integer(as.character(clean_full_candy$age))

# filtering a reasonable age range of 1-100, although a fair range is fairly subjective

filtered_candy <- clean_full_candy %>%
  filter(
    age >= 1 & age <= 100 
  )
  
write_csv(filtered_candy, "clean_data/clean_candy.csv")

  
