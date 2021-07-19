#' Retrieve the tidal epoch
#'
#' For most US tidal stations, the function currently returns
#' `c(start = 1983, end = 2001)` but some stations on the Gulf Coast and Alaska
#' have "anomalously high" sea level rise rates, and so tidal forecasts have
#' been calibrated based on a more recent five year period, usually 2012 through
#' 2016.  For those Stations, this function will return
#' `c(start = 2012, end = 2016)`.
#'
#' The function is especially useful for international stations, where the
#' period used to calculate tidal statistics my be different.  NOAA is expected
#' to adopt a revised tidal datum in 2025.
#'
#' @param .station
#'
#' @return A named list of two integers, representing the starting and ending
#'         years of the official tidal epoch.
#'

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
