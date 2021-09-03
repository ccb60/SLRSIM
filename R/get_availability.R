
#' Retrieve NOAA station details
#'
#' Internal utility function to retrieve data about a specific NOAA tide station
#' from the NOAA metadata API, especially when the station was established, and
#' its timezone offset from UTC.
#'
#' This function is called by other functions that extract specific
#' subcomponents of the metadata. Users should generally call specific metadata
#' access functions.  Every time this function is called, it triggers a new
#' call to the NOAA API, which is wasteful of resources, but under most
#' circumstances access time is minimal.
#'
#' @param .station A number or string specifying the NOAA station for which
#'        to retreive metadata. A list of NOAA stations is available at
#'        https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels. A
#'        user-friendly map interface is available here:
#'        https://tidesandcurrents.noaa.gov/map/index.html?type=TidePredictions.
#'
#' @return A list representation of the NOAA Details metadata, with the following
#' components:
#' \describe{
#' \item{id}{Station ID.}
#' \item{established}{Character string representing date (and time? ) the
#'       station was first established. In "%Y-%m%d %H%M%S" format.}
#' \item{removed}{Character string representing date (and time?) the
#'       station was taken out of service.  For active stations, this is an
#'       empty string. If a value is present, it is in "%Y-%m%d %H%M%S" format.}
#' \item{noaachart}{ID number for the local NOAA chart}
#' \item{timemeridian}{NOAA's metadata tells us (unhelpfully) that this is the
#'       "Time meridian". It usually (always?) has a value of zero for stations
#'       in the continental US. That may only indicate that the following
#'       timezone designation is relative to GMT.}
#' \item{timezone}{A time zone numerical offset. Negative values are WEST of
#'       the prime meridian, in Greenwich England. These are for Standard Time.
#'       Eastern Standard time is offset -5, for example.}
#' \item{origyear}{Despite the name, this gives the date and time of the current
#'       installation. In "%Y-%m%d %H%M%S" format.}
#' \item{self}{the URL used to access the metadata}
#' }
#'
#' @noRd
.call_details <- function(.station) {
  # I imagine this as an internal utility function....
  # it returns the raw list object from the API from which we will extract parts
  # in the related access functions.

  # While the `httr` package allows specification of query strings as lists,
  # the critical addition here is the station, which is not part of the query
  # string, but part of the  base URL.

  base_url <- 'https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations'
  assembled_url <- paste0(base_url, '/', as.character(.station), '/',
                          'details.json')
  r <- httr::GET(assembled_url)

  httr::stop_for_status(r, paste('download details for station', .station))

  return(httr::content(r))
}


#' Retrieve period of operation of a NOAA Tide Station
#'
#' @param .station The code specifying the station of interest.
#'
#' @return A list with two components: `$firstyr` and `$lastyr` for the year
#'         the station was established (not  when its current configuration was
#'         put into place) and pulled from service, respectively. If the station
#'         is stil in service, `$lastyr` will return the current year.
#' @family station information access functions
#' @export
#'
#' @examples
#'  Providence_station <- 8454000
#'  get_availability(Providence_station)
get_availability <- function(.station) {
  # For historical reasons, timezones designated as offsets in R are positive
  # west of the prime meridian, so we need to convert, then assemble the tz.
  # Note that designation of the time zone only affects the internal
  # representation of the time, so of little importance here, where we
  # report only the calendar year.

  # Error checks here
  stopifnot(length(.station) == 1)

  b <- .call_details(.station)
  the_tz <-paste0('Etc/GMT+', as.character(-b$timezone))

  established <- as.POSIXct(b$established, format = "%Y-%m-%d %H:%M:%S",
                            tz = the_tz)
  established <-as.numeric(format(established, '%Y'))

  removed <- b$removed
  if(removed == '') {
    removed <- as.numeric(format(Sys.Date(), '%Y'))
  }  else {
    removed <- as.numeric(format(removed, '%Y'))
  }
  return(list(first_yr = established, last_yr = removed))
}

#' Access station timezone information, either as an offset from UTC or a timezone string
#'
#' Consults the NOAA metadata API for your selected station, and queries it
#' for information on the station's (standard time) timezone offset from UTC.
#'
#' @param .station Code for NOAA tide station.
#' @param .type  Either 'offset' for an integer value offset from UTC, or
#'
#' @return Either an integer  for `.type = 'offset'`) or a string
#'         (`.type = 'string')of specifying a time zone offset, of the form
#'         'Etc/GMT+X', where X is an integer. This string form is likely to
#'         only work correctly for the western hemisphere, and has not been
#'         tested elsewhere.  U.S Eastern Standard Time is offset == -5,
#'         or 'Etc/GMT+5'
#' @family station information access functions
#' @export
#'
#' @examples
#' Providence_station <- 8454000  # Providence, Rhode Island
#' get_tz(Providence_station)
#' get_tz(Providence_station, .type = 'string')
get_tz <- function(.station, .type = c('offset', 'string')) {
  .type <- match.arg(.type)

  # Error checks here
  stopifnot(length(.station) == 1)

  b <- .call_details(.station)
  if (.type =='offset') {
    the_tz<- b$timezone
  } else if (.type == 'string') {
    the_tz <-paste0('Etc/GMT+', as.character(-b$timezone))
  } else {
    stop('Unrecognized .type requested for timezone information.')
  }
  return(the_tz)
}
