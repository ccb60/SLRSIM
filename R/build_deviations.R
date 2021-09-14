#' Retrieve data from NOAA API and calculate deviations between observed and
#' predicted water levels
#'
#' @inheritParams retrieve_data
#'
#' @return A dataframe with columns "datetime" and "water_level" and "deviation"
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
print('in build_deviations()')
}
