## code to prepare `prov_astro` dataset goes here

library(dplyr)
library(data.table)  # for fread to read large files

# Load Observed Tidal Elevations
fn <- 'providence_tides_hourly.csv'
prov_obs <- fread(file.path('data-raw',fn))

prov_obs <-  as_tibble(prov_obs) %>%
  rename(MLLW = `Water Level`,
         theDate =`Date`,
         Hour = Time) %>%
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M"),
         theDate = as.Date(theDate)) %>%
  mutate(Hour  = as.numeric(format(DateTime, '%H')),
         Day   = as.numeric(format(theDate, '%d')),
         Month = as.numeric(format(theDate, '%m')),
         Year  = as.numeric(format(theDate, '%Y'))) %>%
  mutate(MLLW_ft = MLLW * 3.28084) %>%
  relocate(MLLW, MLLW_ft, .after = Year) %>%
  select(-Sigma) %>%
  filter( ! is.na(MLLW))

# Load Astronomical Hindcasts of Tidal Elevations

fn <- 'providence_tides_hourly_predicts.csv'
prov_astro <- read_csv(file.path('data-raw',fn), col_types = cols(Time = col_time('%H:%M'))) %>%
  rename(theDate =`Date`) %>%
  mutate(Hour  = as.numeric(format(DateTime, '%H')),
         Month = as.numeric(format(theDate, '%m')),
         Day   = as.numeric(format(theDate, '%d')),
         Year  = as.numeric(format(theDate, '%Y')),
         Prediction_ft = Prediction * 3.28084) %>%
  relocate(Prediction, Prediction_ft, .after = Year) %>%
  select(-DateTime, -theDate, -Time)

prov_tides <- prov_obs %>%
    inner_join(prov_astro , by = c("Year",  "Month", "Day", "Hour"))

  ## Calculate Deviations Between Predicted and Observed
prov_tides <- prov_tides %>%
    mutate(deviation = MLLW - Prediction)

usethis::use_data(prov_tides, overwrite = TRUE)
