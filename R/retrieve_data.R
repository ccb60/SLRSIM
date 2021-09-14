
#' Send One call to the NOAA API to retrieve hourly water level data
#'
#' `call_api()` places a single call to the NOAA API to retrieve either
#' hourly water level observations or predictions.
#'
#' @param .station Numeric or Character. The code that uniquely identifies a
#'        NOAA water level station.
#' @param .start  An object that inherits from R's `Date` class or a string of
#'        either "yyyymmdd" or "mm/dd/yyyy" format.  The starting date for the
#'        period of data retrieval.
#' @param .stop  An object that inherits from R's `Date` class or a string of
#'        either "yyyymmdd" or "mm/dd/yyyy" format.  The ending date for the
#'        period of data retrieval.  The NOAA API restricts data retrieval of
#'        hourly data to no more than one year.  This function will throw an
#'        error without sending a request to the NOAA API if your request does
#'        not comply with that requirement.
#' @param .which 'observed' or 'predicted'
#' @param .datum Character string, from a limited list of options. Specifies the
#'        vertical datum used for the returned data. "STND" is a
#'        station-specific datum anticipated to be below all observed or
#'        predicted water levels, so al lwater levels wil lbe positive numbers.
#'        "NAVD" is the National Vertical Datum, useful for relating tides to
#'        ground elevations from survey or LIDAR.  The other choices are
#'        standard tidal datums.
#' @param .units  "metric" or "english".  The default is "metric".
#' @param .timefmt 'gmt', 'lst', or 'lst_ldt'.  How would you like the time
#'        coordinate defined?  Choices are for greenwich mean time (also known as
#'        UTC, although that abbreviation does not work here), local standard
#'        time (with no adjustments for daylight savings time), or local clock
#'        time, which can be confusing, since the data returned is a string (not
#'        an internal `POSIXct` or `POSIXlt` object).  Interpretting the data
#'        correctly requires care specifying the timezone.
#' @param .tz An R timezone specification. Used only for converting text-based
#'        times deliverd from the API to POSIXct objects. Defaults to 'UTC', for
#'        consistency with the default `.timefmt = 'gmt'`. For local standard
#'        time, as specified via `.timefmt = 'lst'`, the easiest way is to
#'        specify an offset from UTC. For example, use  'Etc/GMT+5' for the U.S.
#'        East Coast.  For `.timefmt = 'lst_ldt'` you will need to figure out
#'        how to define your local clock time as a time zone. For the U.S East
#'        Coast, `.tz = "America/New_York"` works. See `?timezone`
#'        for more information.
#' @family NOAA water level data access functions
#' @return A dataframe with columns "datetime" and "water_level"
#' @export
#' @examples
#' Providence_station <- 8454000
#' begin_date <- '20200806'
#' end_date   <- '20200810'
#' call_api_observed(Providence_station, begin_date, end_date)
#'
call_api <- function(.station, .start, .stop,
                              .which = c('observed', 'predicted'),
                              .datum = c('MSL', 'MTL',
                                         'MHW', 'MHHW',
                                         'MLLW', 'MLW',
                                         'NAVD', 'STND'),
                              .units = c('metric', 'english'),
                              .timefmt = c('gmt', 'lst', 'lst_ldt'),
                              .tz =  'UTC')  {

  # TODO: Consider whether to automate selection of time zone and drop the
  # TODO: Add code to test if .stop > .start?
  # '.tz' parameter.   We can get the timezone with `get_tz()`


  BASE <-  'https://api.tidesandcurrents.noaa.gov/api/prod/datagetter'


  if (missing(.which)) {
    warning('Choice of what to retreive not specified via .which. ',
            'Defaults to fetching observed data.')
  }
  .which <- match.arg(.which)
  .datum <- match.arg(.datum)
  .units <- match.arg(.units)
  .timefmt <- match.arg(.timefmt)

  # No parameter should have length > 1
  stopifnot(length(.station) == 1)
  stopifnot(length(.start) == 1)
  stopifnot(length(.stop) == 1)

  #check that start dates are of allowed types and convert date
  if (inherits(.start, 'Date')) {
    .start <- format(.start, format = '%Y%m%d')
  }
  else if (unlist(gregexpr('/', .start)) == c(3,6) &&
           nchar(.start) == 10) {
    # could add checks for other malformed dates here -- like impossible
    # months and days, but we pass this through to the API
  }
  else if (nchar(.start) == 8 && length(unlist(gregexpr('[[:digit:]]', .start)))) {
    century <- as.numeric(substr(.start,1,2))
    if (! century >=18 && century <= 20) {
      stop('"YYYYmmdd" requires dates in 1800, 1900s or 2000s')
    }
  }
  else {
    stop('Malformed .start date. ',
         'Use r Date class, "YYYYmmdd" or "mm/dd/YYYY" formated strings')
  }

  #check that stop dates are of allowed types and convert date
  if (inherits(.stop, 'Date')) {
    .stop <- format(.stop, format = '%Y%m%d')
  }
  else if (unlist(gregexpr('/', .stop)) == c(3,6) &&
           nchar(.stop) ==  10) {
    # could add checks for other malformed dates here -- like impossible
    # months and days, but we pass this through to the API
  }
  else if (nchar(.stop) == 8 && length(unlist(gregexpr('[[:digit:]]', .stop))) == 8) {
    century <- as.numeric(substr(.stop,1,2))
    if (! century >=18 && century <= 20) {
      stop('"YYYYmmdd" requires dates in 1800, 1900s or 2000s')
    }
  }
  else {
    stop('Malformed .stop date. ',
         'Use r Date class, "YYYYmmdd" or "mm/dd/YYYY" formated strings')
  }

  parms <- list(
    'application'='SLRSIM',
    'format'='json',
    'station' = .station,
    'datum' = .datum,
    'units' = .units,
    'time_zone' = .timefmt,
    'begin_date' = .start,
    'end_date'= .stop
  )

  if (.which == 'observed') {
    parms['product'] <- 'hourly_height'
  } else if (.which == 'predicted') {
    parms['product'] <- 'predictions'
    parms['interval'] <- 'h'
  }

  r <- httr::GET(BASE, query = parms)
  if (r$status_code == 200) {
    if ( ! 'error' %in% names(r)) {
      # We got data
      if (.which == 'observed') {
        d <- httr::content(r)$data
      } else if (.which == 'predicted') {
        d <- httr::content(r)$predictions
        #browser()
      }
      res <- .lst_2_df(d, tz = .tz)
      return(structure(res,
                       which = .which,
                       station = .station,
                       datum = .datum,
                       timefmt = .timefmt,
                       url = r$url))
    } else{
      # consider if this should be a warning or message
      stop('API call returned status 200 but returned no data.')
    }
  } else {
    stop('API call failed with status code ', r$status_code)
  }

}


#' Retrieve hourly observed or predicted water level from the NOAA API
#'
#' This function encapsulates calling the NOAA API to assemble a data frame
#' and optionally a CSV file containing historic observed water levels or
#' predicted water levels for a NOAA water level station.
#'
#' NOAA publishes coastal water level data for hundreds of water level
#' monitoring stations through an API. The API restricts individual requests for
#' high frequency data to relatively short time periods (generally one month for
#' six minute data, and one year for hourly data). As a result, one needs to
#' marshal repeated API calls to assemble a longer-term record. This function
#' encapsulates the logic to assemble a longer record, by breaking a longer
#' period of time into yearly blocks, and reassembling into a single data frame.
#'
#' The interface here asks the user to specify only the first and last years
#' of the desired data.  This is simpler than specifying an exact first and last
#' date, although it may sometimes return more data than really desired.
#'
#' @inheritParams call_api
#' @param .first_yr  Integer. First year from which to retrieve data.
#' @param .last_yr   Integer. Last year from which to retrieve data.
#'
#' @inherit call_api return
#' @export
#'
#' @examples
retrieve_data <- function(.station, .first_yr, .last_yr,
                          .which = c('observed', 'predicted'),
                          .datum = c('MSL', 'MTL',
                                     'MHW', 'MHHW',
                                     'MLLW', 'MLW',
                                     'NAVD', 'STND'),
                          .units = c('metric', 'english'),
                          .timefmt = c('gmt', 'lst', 'lst_ldt'),
                          .tz =  'UTC') {
  # TODO: Consider whether to automate selection of time zone and drop the
  # '.tz' parameter.   We can get the timezone with `get_tz()`
  # TODO: Add code to test if .stop > .start?
  if (missing(.which)) {
    stop('Must specify either "observed" or "predicted" values with .which.')
  }

  .which <- match.arg(.which)
  .datum <- match.arg(.datum)
  .units <- match.arg(.units)
  .timefmt <- match.arg(.timefmt)

  # No parameter should have length > 1
  stopifnot(length(.station) == 1)
  stopifnot(length(.first_yr) == 1)
  stopifnot(length(.last_yr) == 1)

  # Check that first and last years are O.K.
  # Could add additional checks here
  if (! is.numeric(.first_yr) || ! is.numeric(.last_yr)) {
    stop("Parameters '.first_yr' and '.last_yr' must be numeric.")
    }
  if (! .first_yr == as.integer(.first_yr) ||
      ! .last_yr == as.integer(.last_yr)) {
    stop("Parameters '.first_yr' and '.last_yr' must be integers.")
  }

  # Don't ask for years that don't exist
  avail <- get_availability(.station)
  if (.first_yr < avail$first_yr) {
    message('Data not available before ', avail$first_yr)
    .first_yr <- avail$first_yr
  }
  if(.last_yr > avail$last_yr) {
    message('Data not available after ', avail$last_yr)
    .last_yr <- avail$last_yr
  }

  # Check if the timezone makes sense
  # consider if this should be a warning? We could recover by using get_tz()
  if(! .is_timezone(.tz)) {
    stop('Timezone specification not recognized.')
  }

  out_data <- data.frame()

  for (year in seq(.first_yr, .last_yr, by = 1)) {
        message (year, ' ')
        begindate <- paste0(as.character(year), '0101')
        enddate <- paste0(as.character(year), '1231')
        next_data <- call_api(.station, begindate, enddate,
                                      .which = .which,
                                      .datum = .datum,
                                      .units = .units,
                                      .timefmt = .timefmt,
                                      .tz =  .tz)
        out_data <- dplyr::bind_rows(out_data, next_data)
  }
  if(.which == 'predicted') {
    out_data <- out_data %>%
      dplyr::rename(predicted = water_level)
  }
  return(structure(out_data,
                   which = .which,
                   station = .station,
                   datum = .datum,
                   timefmt = .timefmt))
}
