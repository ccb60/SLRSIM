#' Retrieve NOAA Long-Term SLR Rate Estimates
#'
#' This function returns NOAA's long-term estimates of sea level rise
#' for a specific water level monitoring station, identified by NOAA's unique
#' identifiers, along with appropriate metadata.
#'
#' The NOAA API can, in principal return data for more than one station if there
#' are multiple matches. This function does not yet handle that situation
#' gracefully. It merely returnsan error.  Users are encouraged to make sure
#' their search selection of `.station` an `.affil` return data for only a
#' single station.
#'
#' @param .station  Unique identifier for station of interest. See
#'        https://tidesandcurrents.noaa.gov/stations.html?type=Water+Levels
#' @param .affil Should matches be restricted to US stations, or global network
#'        stations. Default is 'us'.  If 'all' is specified, then both networks
#'        are searched.
#'
#' @return A list containing specifics about NOAA's long-term SLR estimates.
#'
#' \describe{
#'   \item{stationId}{Unique NOAA id for accessing the Station.}
#'   \item{stationName}{Name of Station.}
#'   \item{affil}{"US"or "Global"}
#'   \item{latitude}{Position of Station}
#'   \item{longitude}{Position of Station}
#'   \item{trend}{Point estimate of sea level rise trend.}
#'   \item{trendError}{Standard error of trend estimate.}
#'   \item{units}{Apparently always "mm/yr"?}
#'   \item{startDate}{Start of period over which trend was calculated.}
#'   \item{endDate}{End date of period over which trend was caclulated.}
#' }
#'
#' @export
#'
#' @examples
#' portland_id <- 8418150
#' sl_list <- get_sl_trend(portland_id)
#' names(sl_list)
#' paste(sl_list$trend, '+/-', sl_list$trendError, 'sl_list$units')
#'
get_sl_trend <- function (.station, .affil = c('us', 'global', 'all')) {
  slttrendurl <- 'https://api.tidesandcurrents.noaa.gov/dpapi/prod/webapi/product/sealvltrends.json'

  stopifnot(length(.station) == 1)

  .affil = match.arg(.affil)

  params = list(station = .station)
  if (tolower(.affil) != 'all') {
    params['affil'] <-  .affil
  }

  r <- httr::GET(slttrendurl, query = params)
  # browser()
  httr::stop_for_status(r, paste('download SLR Trend information file for station', .station))

  if (httr::content(r)[[1]] == 0) {
    stop('Station not found.')
  }
  if (httr::content(r)[[1]] > 1) {
    stop('Multiple stations matching .station and .affil found.')
  }

  return(httr::content(r)[[2]][[1]])
}


