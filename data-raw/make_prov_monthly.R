## code to prepare `prov_astro` dataset goes here

library(dplyr)
library(readr)

# Load Observed Tidal Elevations
fn <- 'providence_tides_monthly.csv'
prov_monthly <- read_csv(file.path('data-raw',fn),
                         col_types = 'ii---n------------') %>%
  mutate(MidDate = as.Date(paste0(Year,'/', Month,'/',15))) %>%
  relocate(MidDate, .after= Month) %>%
  mutate(MSL_ft = MSL *  3.28084) %>%
  mutate(MSL_mm = MSL *  1000) %>%
  filter( ! is.na(MSL))

usethis::use_data(prov_monthly, overwrite = TRUE)
