#' Monthly Mean Sea Level 1938 through 2020 for Providence, Rhode Island
#'
#' A dataset containing monthly mean water levels at the NOAA tide gauge in
#' Providence, Rhode Island. Station ID: 8454000. Data is available from
#' Providence for the period from 1938 through 2021, with
#' some gaps in the data.  This data set contains raw (not seasonally adjusted)
#' mean water levels.  It is included here to compare to NOAA's seasonally
#' adjusted data, presented in the `prov_meantrend` data.
#'
#' Values are expressed relative to the most recent mean sea level (MSL)
#' datum. At the time of data download, the tidal epoch used for calculating the
#' tidal datums was from 1983 through 2001.
#'
#' Data released by NOAA was downloaded directly from a NOAA API using the
#' URL provided below. Only a subset of the data provided by NOAA is used here.
#'
#' @format A data frame with 871 rows and 6 variables:
#' \describe{
#'   \item{Year}{Year of mean sea level observation}
#'   \item{Month}{Month of mean sea level Observation}
#'   \item{MidDate}{Date of the fifteenth of the month}
#'   \item{MSL}{mean sea level, in meters, here }
#'   \item{MSL_ft}{mean sea level, converted to feet}
#'   \item{MSL_mm}{mean sea level, converted to millimeters}
#' }
#' @source \url{https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=19000729&end_date=20210729&station=8454000&product=monthly_mean&datum=MSL&time_zone=lst&units=metric&format=csv}
"prov_monthly"
