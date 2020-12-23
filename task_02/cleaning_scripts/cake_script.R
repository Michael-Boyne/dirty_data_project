# cake cleaning script -------------------------------------------------------

# library loadout
library(tidyverse) 
library(janitor)

# load the data with clean column names
cake_ingredients <- read_csv("data/cake/cake-ingredients-1961.csv") 
cake_code <- read_csv("data/cake/cake_ingredient_code.csv")

# cleaning jobs for cake_code
# fix sour cream row
clean_cake_code <- cake_code %>%
  mutate(ingredient = recode(ingredient, "Sour cream cup" = "Sour cream")) %>%
  replace_na(list(measure = "cup"))

# cleaning jobs for cake_ingredients
clean_cake_ingredients <- cake_ingredients %>%
  pivot_longer(AE:ZH, names_to = "code", values_to = "quantity") %>%
  rename(cake = Cake) %>%
  left_join(clean_cake_code, by = "code") %>%
  drop_na(quantity) %>%
  mutate(cake = str_trim(cake))

# writing clean csv

write_csv(clean_cake_ingredients, "clean_data/cake.csv")