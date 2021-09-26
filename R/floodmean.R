#' Calculate Mean Days per Year with Flood Events From Historical Data
#'
#' This function takes high frequency data on tidal elevations (typically
#' hourly or more frequently) and counts the number of days each
#' year where the water level (`.wl`) exceeded a specified flood
#' elevation (`.fldlvl`) at least once.  It calculates the mean annual
#' number of days with observed flooding, the daily probability of flooding,
#' and a scaled estimate of days with flooding, scaled to account for days with
#' no water level data.
#'
#' @inheritParams floodcounts
#'
#' @return A 3 (row) x 2 (column) array containing `Mean` and `Var` for
#' *  `Daily Probability` of flooding,
#' *  Annual `Flood Days` observed, and
#' *  Annual `Scaled Flood Days`, which are scaled to account for days each year
#'    with no data.
#'
#' @family Flood frequency analysis functions
#' @export
#'
#' @examples
#' old <- prov_tides[prov_tides$Year >= 1940 & prov_tides$Year < 1950,]
#' new <- prov_tides[prov_tides$Year >= 2010 & prov_tides$Year < 2020,]
#' floodmean(old, DateTime, MLLW, 1.987)
#' floodmean(new, DateTime, MLLW, 1.987)
#'
floodmean <- function(.data, .dt, .wl, .fldlvl) {

  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)

  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(length(.fldlvl) == 1)
  stopifnot(is.numeric(.fldlvl))
  stopifnot(inherits(dt, 'POSIXct'))

  # create a dataframe, for convenience use of the
  df <- dplyr::tibble(theDT = dt, obs_wl = wl) %>%
    dplyr::mutate(theDate = as.Date(theDT),
                  theYear = as.numeric(format(theDate, format = '%Y'))) %>%
    dplyr::select(-theDT)
  Years <- length(unique(df$theYear))

  results <- df %>%
    dplyr::group_by(theDate) %>%
    dplyr::summarize(theYear = dplyr::first(theYear),
                     exceeded = any(obs_wl > .fldlvl),
                     n = sum(! is.na(exceeded)),    # Count days with ANY data...
                     .groups = 'drop') %>%
    dplyr::group_by(theYear) %>%
    dplyr::summarize(days = sum(n),
                     flood_days = sum(exceeded),
                     daily_p_flood = flood_days / days,
                     floods_p_yr = daily_p_flood *
                       dplyr::if_else(theYear %% 4 == 0 & (! theYear %% 100 == 0), 365, 366),
                     .groups = 'drop') %>%
    dplyr::relocate(daily_p_flood, flood_days, floods_p_yr, .after = days) %>%
    dplyr::summarize(dplyr::across(dplyr::contains('flood'),
                            c(mean = mean, Var = var), na.rm = TRUE))

  results <-  unlist(results)
  results <- matrix(results, nrow = 3, byrow = TRUE)
  colnames(results) <- c('Mean', 'Var')
  rownames(results) <- c('Daily Probability', 'Flood Days', 'Scaled Flood Days')
  return(structure(results,
                   Years = Years))
}
