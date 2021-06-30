## code to prepare `DATASET` dataset goes here
# I am concerned that I am explicitly calling an external package here.
# It is probably o.k, because this folder is not included in the final
# package, but it feels inconsistent.

library(dplyr)
library(readr)
fn <- '8454000_meantrend.csv'

# The following generates parsing warnings, because of trailing commas.
prov_meantrend  <- read_csv(file.path('data-raw',fn),
                      col_types = cols(Year	= col_integer(),
                                       Month = col_integer(),
                                       Monthly_MSL = col_double(),
                                       Linear_Trend = col_double(),
                                       High_Conf.  = col_double(),
                                       Low_Conf.  = col_double()
                      )) %>%
  rename(MSL = Monthly_MSL) %>%
  mutate(MidDate = as.Date(paste0(Year,'/', Month,'/',15))) %>%
  relocate(MidDate, .after= Month) %>%
  mutate(MSL_ft = MSL *  3.28084) %>%
  select(-Linear_Trend, -Low_Conf., -High_Conf.)

usethis::use_data(prov_meantrend, overwrite = TRUE)

