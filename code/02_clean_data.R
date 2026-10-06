# 02_clean_data.R -- keep real countries, drop missing values
library(here)
library(tidyverse)

raw <- read_csv(here("data/raw/wdi_raw.csv"), show_col_types = FALSE)

clean <- raw |>
  filter(region != "Aggregates",                 # drop World, regions, income groups
         income %in% c("High income", "Upper middle income",
                       "Lower middle income", "Low income")) |>
  drop_na(gdppc, lifeexp, pop) |>
  select(iso3c, country, year, region, income, gdppc, lifeexp, pop)

write_csv(clean, here("data/processed/wdi_clean.csv"))
message("Clean sample: ", n_distinct(clean$country), " countries, ",
        min(clean$year), "-", max(clean$year))
