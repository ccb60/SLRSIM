###############################
#  utils
###############################
BASE <-  'https://api.tidesandcurrents.noaa.gov/api/prod/datagetter'

.station <- '8454000'
.start <- '20200801'
.stop <- '20200801'

parms <- list(
  'product' = 'hourly_height',
  'application'='SLRSIM',
  'format'='json',
  'station' = .station,
  'datum' = 'MSL',
  'units' = 'metric',
  'time_zone' = 'GMT',
  'begin_date' = .start,
  'end_date'= .stop
)

r <- httr::GET(BASE, query = parms)
q <-  httr::content(r)$data [1:5]
p <- SLRSIM::.lst_2_df(q)
saveRDS(q, 'tests/testthat/test_list.RDS')
saveRDS(p, 'tests/testthat/test_obs_df.RDS')

##############################
# retrieve_data
##############################

parms['product' = 'predictions']
r <- httr::GET(BASE, query = parms)
q <- httr::content(r)$data [1:5]
p <- SLRSIM::.lst_2_df(q)
saveRDS(p, 'tests/testthat/test_pred_df.RDS')

