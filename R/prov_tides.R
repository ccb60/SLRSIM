#' Sea Level Observations and Predictions for Providence, Rhode Island
#'
#' A dataset containing hourly observations, and hourly predictions of tidal
#' elevations at the NOAA tide gauge in Providence, Rhode Island.
#' Station ID: 8454000.
#'
#' Data is available from Providence for the period from 1938 through 2021, with
#' some gaps in the data. Data included here omits tide predictions for dates
#' and times without corresponding observational data.  Released version of the
#' package may include a subset of the data to make the package leaner.
#'
#' Values are expressed relative to the most recent mean lower low water tidal
#' datum.  Water depths on navigational charts are ordinarily expressed relative
#' to mean lower low water.  At the time of data download, the tidal epoch used
#' for calculating the tidal datums was from 1983 through 2001.
#'
#' Data as released by NOAA was downloaded directly from a NOAA API using
#' python scripts.  Details on the API are available from the NOAA web page,
#' \url{https://tidesandcurrents.noaa.gov/api/}. Similar data can be downloaded
#' manually from the official station webpage, listed as the data source, below.
#'
#' We combined records of observed water levels and predicted water levels into
#' a single large data frame.
#'
#' @format A data frame with 630,255 rows and 11 variables:
#' \describe{
#'   \item{DateTime}{Year of mean sea level observation}
#'   \item{theDate}{Month of mean sea level Observation}
#'   \item{Hour}{Hour of observation, as an integer in the range 1:24}
#'   \item{Day}{Day of the month of the observation, as an integer}
#'   \item{Month}{Month of observation, as an integer in the range 1:12}
#'   \item{Year}{Year of observation, as an integer}
#'   \item{MLLW}{Observed tidal elevation, in meters above mean lower low water}
#'   \item{MLLW_ft}{Observed tidal elevation, in feet above mean lower low water}
#'   \item{Prediction}{Predicted tidal elevation, in meters above mean lower low
#'         water }
#'   \item{Prediction_ft}{Predicted tidal elevation, in feet above mean lower
#'         low water}
#'   \item{deviation}{Difference between observed and predicted tidal heights.
#'         positive values imply observed sea level was above predicted levels.}
#' }
#' @source \url{https://tidesandcurrents.noaa.gov/stationhome.html?id=8454000}
"prov_tides"
