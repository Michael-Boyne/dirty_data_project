# RWA cleaning script --------------------------------------------------------

# library loadout
library(tidyverse) 
library(janitor)

# load the data with clean column names
rwa <- read_csv("data/rwa.csv")
