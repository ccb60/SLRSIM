#' Retrieve data from NOAA API and calculate deviations between observed and
#' predicted water levels
#'
#' @inheritParams retrieve_data
#'
#' @return A dataframe with columns "datetime", "water_level" and "deviation".
#'         The values in "deviation" are the difference in observed and
#'         predicted tides. Positive values mean observed water level was higher
#'         than predicted.
#' @export
#'
#' @examples
build_deviations <- function(.station, .first_yr, .last_yr,
                             .datum = c('MSL', 'MTL',
                                        'MHW', 'MHHW',
                                        'MLLW', 'MLW',
                                        'NAVD', 'STND'),
                             .units = c('metric', 'english'),
                             .timefmt = c('gmt', 'lst', 'lst_ldt'),
                             .tz =  'UTC') {

  .datum <- match.arg(.datum)
  .units <- match.arg(.units)
  .timefmt <- match.arg(.timefmt)

  #If user does not specify the years, get what is available
  if(missing(.first_yr) || missing(.last_yr)) {
    timespan <- get_availability(.station)
    if(missing(.first_yr)) .first_yr <- timespan$first_yr
    if(missing(.last_yr)) .last_yr <- timespan$last_yr
  }

  # No parameter should have length > 1
  stopifnot(length(.station) == 1)
  stopifnot(length(.first_yr) == 1)
  stopifnot(length(.last_yr) == 1)
  message('Retrieving observed water levels....')
  observed <- retrieve_data(.station  = .station,
                            .first_yr = .first_yr,
                            .last_yr  = .last_yr,
                            .which    = 'observed',
                            .datum    = .datum,
                            .units    = .units,
                            .timefmt  = .timefmt,
                            .tz       =  .tz)
  message('Retrieving predicted water levels....')
  predicted <- retrieve_data(.station  = .station,
                             .first_yr = .first_yr,
                             .last_yr  = .last_yr,
                             .which    = 'predicted',
                             .datum    = .datum,
                             .units    = .units,
                             .timefmt  = .timefmt,
                             .tz       =  .tz)
  message('Calculating deviations....')
  deviations <- dplyr::full_join(observed, predicted, by = 'datetime') %>%
    dplyr::mutate(deviations = water_level - predicted ) %>%
    dplyr::select(-predicted)
  attr(deviations, 'which') <- NULL
  message('Done!')
  return(deviations)
}
