# Todo: Decide whether to provide a default flood level as HAT, which would
# require considering the units and datum.

#' Count Number of Floods and Estimate Daily Risk and Annual Flood Frequency
#'
#' This function splits a data set by day and counts the number of days in
#' the data set where the water level (`.wl`) exceeded a specified flood
#' elevation (`.fldlvl`) at least once.  It then calculates the proportion of
#' days with flooding (as an estimate of daily flood risk) and the expected
#' number of days with flood events per year (by scaling by the daily risk
#' by 365.25 days per year).
#'
#' There is no effort to account for partial data within days. This function is
#' principally intended for exploratory data review or as a building block for
#' more complex analyses. The function, `floodmeans()` provides closely
#' related statistics, averaged on a year-by-year basis, and offering
#' estimates of year to year variability. `floodcounts()` provides the raw
#' annual counts of the number of days with flooding.
#'
#' @inheritParams floodcounts
#' @return A named numeric vector consisting of the values `days`, `flood_days`,
#'         `daily_p_flood`, and `floods_p_yr`.
#'
#' @family Flood frequency analysis functions
#' @export
#'
#' @examples
#' old <- prov_tides[prov_tides$Year >= 1940 & prov_tides$Year < 1950,]
#' new <- prov_tides[prov_tides$Year >= 2010 & prov_tides$Year < 2020,]
#' floodcount(old, DateTime, MLLW, 1.987)
#' floodcount(new, DateTime, MLLW, 1.987)
#'
floodfreq <- function(.data, .dt, .wl, .fldlvl) {

  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)

  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(length(.fldlvl) == 1)
  stopifnot(is.numeric(.fldlvl))
  stopifnot(inherits(dt, 'POSIXct'))

  # create a dataframe, for convenience use of the
  response <- dplyr::tibble(theDT = dt, obs_wl = wl) %>%
    dplyr::mutate(theDate = as.Date(theDT)) %>%
    dplyr::select(-theDT)  %>%
    dplyr::group_by(theDate)  %>%
    dplyr::summarize(exceeded = any(obs_wl > .fldlvl),
                     .groups = 'drop') %>%
    dplyr::summarize(days = sum(! is.na(exceeded)),
                     flood_days = sum(exceeded, na.rm = TRUE),
                     daily_p_flood = flood_days/days,
                     floods_p_yr = 365.25 * daily_p_flood,
                     .groups = 'drop')
  return(unlist(response))
}



