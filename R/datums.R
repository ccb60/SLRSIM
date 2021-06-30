#' Retrieve NOAA Datums Metadata
#'
#' Internal utility function to retrieve data about a specific NOAA tide station
#' from the NOAA metadata API.
#'
#' This function is called by other functions that extract specific
#' subcomponents of the metadata. users should generally call specific metadata
#' accessing functions.  Every time this function is called, it triggers a new
#' call to the NOAA API, which is wasteful of resources, but under most
#' circumstances access time is minimal. Coding a mechanism for caching results
#' would add greatly to code complexity, for little gain.
#'
#' @param .station A number or string specifying the NOAA station for which
#'        to retreive metadata. A list of NOAA stations is available at
#'        https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels. A
#'        user-friendly map interface is available here:
#'        https://tidesandcurrents.noaa.gov/map/index.html?type=TidePredictions.
#' @param .units Should values be returned in metric (default) or english units?
#'
#' @return A list representation of the NOAA Datums metadata, with the following
#' components:
#' \describe{
#' \item{accepted}{Date of acceptance of the current datums}
#' \item{superseded}{date datums were superseded?}
#' \item{epoch}{Years defining the tidal epoch.  1983-2001 for most stations for
#'       now}
#' \item{units}{english or metric?}
#' \item{OrthometricDatum}{Prefered orthometric datum for the station.
#'       Orthometric datums use the earth's gravity field as their datum.
#'       Will usually be NAVD88, but several others have been used historically,
#'       especially NGVD29. These can loosely be considered to correspond to the
#'       public perception of "sea level", as these are often used to reference
#'       away from the coast.}
#' \item{datums}{a list of names lists, containing details of the datums at the
#'       station.  list of available datums may vary.  Common ones include:
#'       STND (Station Datum, MHW (Mean High Water), MSL (Mean Sea Level),
#'       MLLW (mean Lower Low Water), and NAVD88 (North American Vertical Datum
#'       1988). List also includes various tidal intervals defining tidal
#'       ranges and size of differences between higher and lower tides of each
#'       day, etc.  Brief descriptions of datums are available on NOAA station
#'       home pages.}
#' \item{LAT}{Lowest astronomical tide. Elevation of lowest tide predicted for
#'       the tidal epoch The list also includes items "LATdate" and "LATtime",
#'       with the obvious meaning, identifying the date and time of the highest
#'       predicted tide.}
#' \item{HAT}{Highest astronomical tide. Elevation of highest tide predicted for
#'       the tidal epoch. Items "HATdate" and "HATtime" are also available.}
#' \item{min}{Lowest observed tide, not necessarily during the tidal epoch
#'        (With "mindate" and "mintime").}
#' \item{max} {Highest observed tide, not necessarily during the tidal epoch
#'        (With "maxdate" and "maxtime").}
#' \item{disclaimers}{Lost of disclaimers.  usually empty.}
#' \item{DatumAnalysisPeriod}{Alternative representation of tidal epoch, with
#'       complete dates, not just years. Not sure whether this always matches
#'       what one would expect from the epoch.}
#' \item{ctrlStation}{Related Control Station. In the case of tidal stations
#'       with partial records, This points to a nearby station with more
#'       complete record that helps define harmonic constituents for tidal
#'       predictions.}
#' \item{self}{URL of station metadata.}
#' }
#'
#' @noRd
.call_datums <- function(.station, .units = 'metric') {
  # I imagine this as an internal utility function....
  # it returns the raw list object from the API from which we will extract parts
  # in the related access functions.

  # While the `httr` package allows specification of query strings as lists,
  # the critical addition here is the station, which is not part of the query
  # string, but the base URL.
  if(! .units %in% c('metric', 'english'))
    stop(".units must be either 'metric' or 'english'.")

  base_url <- 'https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations'
  assembled_url <- paste0(base_url, '/', as.character(.station), '/',
                          'datums.json?units=', .units)
  r <- httr::GET(assembled_url)

  httr::stop_for_status(r, paste('download datums file for station', .station))

  return(httr::content(r))
}


#' Retrieve a named vector of tidal datums
#'
#' Retrieve information from NOAA on tidal datums and certain statistics
#' describing the tidal regime at NOAA tide stations.
#'
#' Each tidal Station in the NOAA Station network measures (or estimates) water
#' elevation at regular intervals (usually every six minutes, or ten times an
#' hour). Raw elevations are referenced to a station-specific reference
#' elevation, called the Station Datum.  Usually, however, we want elevations
#' expressed in units that can be interpreted in relation to tidal
#' charts, topographic maps, surveyor's benchmarks or statistical summaries of
#' water level, such as the mean height of all high tides.
#'
#' The values returned by this function provide information on the
#' relative elevation of different elevation reference frames, and thus
#' facilitate interconversions.
#'
#' The return value here includes all values provided by NOAA as part of their
#' list of datums. The list of available datums may not always remain the same.
#' Most tidal datums are defined as means over the period of the tidal epoch. A
#' list of datums with descriptions is available from NOAA here:
#' https://tidesandcurrents.noaa.gov/datum_options.html. Not all datums defined
#' there are available via this function.
#'
#' The primary values available include the following:
#' \describe{
#' \item{MHHW}{Mean Higher High Water (mean of the higher high tide each day)}
#' \item{MHW}{Mean High Water (mean of all high tides)}
#' \item{DTL}{Mean Diurnal Tide Level (mean of MHHW and MLLW. Sometimes MDTL)}
#' \item{MTL}{Mean Tidel Level.  Mean of MHW and MLW. (MHW + MLW)/2)}
#' \item{MSL}{Mean Sea Level.  Mean of hourly tidal observations over the tidal
#'       epoch.}
#' \item{MLW}{Mean Low Water}
#' \item{MLLW}{Man Lower Low Water}
#' \item{GT}{Greater Diurnal Range. (MHHW - MLLW)}
#' \item{MN}{Mean Range of Tide. (MHW - MLW)}
#' \item{DHQ}{Mean Diurnal High Water Inequality.  Mean difference between the
#'       height of two daily high tides}
#' \item{DLQ}{Mean Diurnal Low Water Inequality.  Mean difference between the
#'       height of two daily low tides}
#' \item{HWI}{Greenwich High Water Interval (hours). The average interval
#'       between the moon's transit over the Greenwich meridian and the
#'       following high water at a location}
#' \item{LWI}{Greenwich High Water Interval (hours). The average interval
#'       between the moon's transit over the Greenwich meridian and the
#'       following low water at a location}
#' \item{NAVD88}{North American Vertical Datum of 1988}
#' }
#'
#' @param .station A number or string specifying the NOAA station for which
#'        to retreive metadata. A list of NOAA stations is available at
#'        https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels. A
#'        user-friendly map interface is available here:
#'        https://tidesandcurrents.noaa.gov/map/index.html?type=TidePredictions.
#' @param .units Should values be returned in metric (default) or english units?
#'
#' @return
#' A named vector of numerical values.  The names are the abbreviations for the
#' tidal datum, facilitating data access.
#'
#' The values are as provided by NOAA. The list includes several statistics of
#' the tidal regime that are not reference elevations, but provide other
#' information about the local tidal regime.
#'
#' All elevation datums are referenced to the (arbitrary) Station Datum. Not all
#' datums available on Station web pages are included.  In particular, HAT, LAT,
#' max and min tides are not included here. See the related `get_hat()` function
#' for access to HAT.
#'
#' @family Datum Access
#' @export
#'
#' @examples
#' # Retreive datums for Portland, Maine
#' portland_id <- 8418150
#' get_datums(portland_id)
get_datums <- function(.station, .units = 'metric'){
  datums <- .call_datums(.station, .units)
  ds_list <- datums$datums

  ds <- sapply(ds_list, function(x) x[3])  # better to  be explicit about conversion?
  names(ds) <- as.vector(lapply(ds_list, function(x) x$name))
  return(ds)
}

#' Retrieve the value of the Highest Astronomical Tide (HAT)
#'
#' @param .station A number or string specifying the NOAA station for which
#'        to retreive metadata. A list of NOAA stations is available at
#'        https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels. A
#'        user-friendly map interface is available here;
#'        https://tidesandcurrents.noaa.gov/map/index.html?type=TidePredictions.
#' @param .units Should values be returned in metric (default) or english units?
#' @param .base  The base datum against which to reference HAT. Defaults to MSL,
#'        with a warning.
#'
#' @return A decimal value, representing the HAT value at the selected station.
#' @family Datum Access
#' @export
#'
#' @examples
#' # Retrieve HAT for Portland, Maine
#' portland_id <- 8418150
#' get_hat(portland_id)                   # 2.134, with a warning
#' get_hat(portland_id, .base = 'MSL')    # 2.134, no warning
#' get_hat(portland_id, .base = 'MLLW')   # 3.64
get_hat <- function (.station, .units = 'metric',
                     .base = c('MSL', 'STND', 'MHHW', 'MHW', 'DTL', 'MTL', 'MLW',
                               'MLLW', 'NAVD88')) {
  stopifnot(inherits(.base, 'character'))
  if (missing(.base)) {
    warning('Base datum not specified, defaulting to MSL.\n')
  }
  .base <- match.arg(.base)
  # browser()
  ds <-.call_datums(.station, .units)

  raw_HAT = ds$HAT
  raw_datums <- ds$datums
  names(raw_datums) <- as.vector(lapply(raw_datums, function(x) x$name))
  base_datum = raw_datums[[.base]]$value

  return(ds$HAT - base_datum)
}

#' Retrieve the tidal epoch
#'
#' @param .station
#'
#' @return A named list of two integers, representing the starting and ending
#'         years of the official tidal epoch.
#'
#' @family Datum Access
#' @export
#'
#' @examples
#' Retrieve HAT for Portland, Maine
#' portland_id <- 8418150
#' get_epoch(portland_id)   # c(start = 1983, end = 2001)
get_epoch <- function(.station){
  ds <-.call_datums(.station, .units = 'metric')
  epoch = as.numeric(strsplit(ds$epoch, '-')[[1]])
  names(epoch) <- c('start', 'end')

  return(epoch)
}
