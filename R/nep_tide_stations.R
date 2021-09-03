#' Codes for some tide stations in or near National Estuary Program study areas
#'
#' A tibble containing numeric codes for selected tide stations in or near the
#' study areas of National Estuary Programs. This is a small subset of available
#' tidal water level station in the U.S. with both observed and predicted tide
#' data available.
#'
#' @format A data frame containing the NOAA code numbers and locationsfor sixty
#'        tide stations.
#' \describe{
#'   \item{code}{numerical (seven digit) code for each tide station}
#'   \item{location}{text name of the location of the tide station}
#'   \item{state}{Standard two letter abbreviation of state or territory name}
#'   \item{nep}{Short name of related National Estuary Program}
#'   }
#'   @source(https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels)
'nep_tide_stations'
