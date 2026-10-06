# 01_get_data.R -- download World Bank WDI data programmatically
library(WDI)
library(here)
library(readr)

indicators <- c(
  gdppc   = "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP (constant 2021 intl $)
  lifeexp = "SP.DYN.LE00.IN",     # Life expectancy at birth, total (years)
  pop     = "SP.POP.TOTL"         # Total population
)

raw <- WDI(country = "all", indicator = indicators,
           start = 1990, end = 2023, extra = TRUE)

write_csv(raw, here("data/raw/wdi_raw.csv"))
message("Saved ", nrow(raw), " rows to data/raw/wdi_raw.csv")
