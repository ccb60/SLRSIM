#' Monthly  Mean Tidal Elevations for Providence, Rhode Island
#'
#' A NOAA sea level trend data set for Providence, Rhode Island.
#'
#' A dataset containing monthly adjusted mean sea levels, for Providence, Rhode
#' Island, from 1938 through early 2021.
#'
#' Values are expressed relative to the most recent mean sea level datum.  At
#' the time of data download, the tidal epoch used for calculating the tidal
#' datums was from 1983 through 2001.
#'
#' Data as released by NOAA for evaluation of long-term sea level trends,
#' adjusted to account for seasonal phenomena. Monthly values are "monthly mean
#' sea level without the regular seasonal fluctuations due to coastal ocean
#' temperatures, salinities, winds, atmospheric pressures, and ocean currents."
#' Original source included NOAA generalized least squares regression results,
#' not needed here.
#'
#' Seasonally adjusted data used by NOAA for analysis of sea level trends
#' is not available through the NOAA tides APIs, but can be accessed manually
#' via the web page associated with each station ID.
#'
#' @format A data frame with 868 rows and 5 variables:
#' \describe{
#'   \item{Year}{Year of mean sea level observation}
#'   \item{Month}{Month of mean sea level Observation}
#'   \item{MidDate}{Date of the fifteenth of the month}
#'   \item{MSL}{mean sea level, in meters, here }
#'   \item{MSL_ft}{mean sea level, converted to feet}
#'   \item{MSL_mm}{mean sea level, converted to millimeters}
#' }
#' @source \url{https://tidesandcurrents.noaa.gov/sltrends/sltrends_station.shtml?id=8454000}
"prov_meantrend"
