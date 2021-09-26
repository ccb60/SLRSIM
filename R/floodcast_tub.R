

# TODO:: Decide whether to provide a default flood level, which would
# require considering the units and datum.
# TODO: Allow finer control over labeling of outputs?  The existing row labels
# are fairly ugly.


#' Forecasting Coastal Flooding with a "Bathtub" Model
#'
#' Calculate estimated future frequency of flooding by adding a fixed "sea level
#' rise offset" to past water levels, and counting up the number of days with at
#' least one observation above a specified flood elevation.
#'
#' `floodcast_tub()` conducts the analysis based on data provided by the user.
#'
#' `floodcast_tub_lookup()`  downloads data for a station of interest and
#'     then conducts the analysis based on the downloaded data.
#'
#' The method follows an idea we were introduced to via the work of Peter
#' Slovinsky, of the Maine Geological Survey.  MGS has estimated future flooding
#' risk at a variety of locations around Maine by adding a fixed SLR value to
#' the historical record, and showing how frequent flooding would have been if
#' base sea levels had been that much higher in the past. This provides a ready,
#' and readily understood, estimate of impact of SLR on frequency of flooding.
#'
#' Our method, while conceptually identical, is slightly different, as we count
#' up the number of __days__ with predicted flooding, rather than hours with
#' flooding. The "days" are calendar days, defined on the same timezone as the
#' source datetime parameter, `.dt`.)
#'
#' There are, of course, many other metric we could use to evaluate frequency of
#' future flooding, such as the number of flood events (continuous periods above
#' flood elevation), or the number of tides with flooding.  We took an approach
#' that focuses on days with flooding in part for computational convenience, and
#' in part because it corresponds well to how communities and individuals are
#' likely to experience "nuisance" tidal flooding.
#'
#'
#' @inheritParams floodcounts
#' @param .fldlvl Flood level.  A single numeric value, that defines what
#'                constitutes a flood event. Must be specified in
#'                `floodcast_tub()`. In `floodcast_tub_lookup()`, will default
#'                to the "Highest Astronomical Tide" elevation, or HAT for the
#'                selected tide station.
#' @param .slr    Sea level rise estimate. A numeric vector containing one or
#'                more values. What sea level rise scenarios should the analysis
#'                be based on?  Selection of appropriate sea level rise
#'                estimates should be based on your time horizon and risk
#'                tolerance. NOAA forecasts of SLR for 2100 vary from 0.3
#'                (almost certain) to 2.5 (highly unlikely) meters.
#' @param .wl_update A single numeric value that adjusts for any changes in
#'                sea level from the tidal epoch to the present day.
#'
#' @family Flood frequency analysis functions
#' @seealso floodcast_arima
#' @export
#'
#' @examples

floodcast_tub <- function(.data, .dt, .wl, .slr, .fldlvl, .wl_update = 0) {
  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)
  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(is.numeric(.slr))
  stopifnot(length(.fldlvl) == 1 && is.numeric(.fldlvl))
  stopifnot(length(.wl_update) == 1 && is.numeric(.wl_update))
  stopifnot(inherits(dt, 'POSIXct')  || inherits(dt, 'Date'))

  #create a dataframe, to use `dplyr`` functions to conduct analysis
  result <- tibble(dt = dt, wl = wl) %>%
    mutate(the_date = as.Date(dt, tz = attr(dt, 'tzone')),
           year    = as.numeric(format(the_date, format = '%Y')))

  if (missing(.slr) || (length(.slr) == 1  && is.na(.slr[[1]]))) {
    # Without .slr scenarios, we fill in with the null SLR case
    result <- result %>%
      mutate(slr_0 = wl > .fldlvl)
  }
  else {
    # we work through each slr scenario and calculate related data
    for (n in seq_along(.slr)) {
      result[[paste0('slr_', .slr[n])]] <-  result$wl + .slr[[n]]
      result[[paste0('exceeds_', .slr[n])]] <- result[[paste0('slr_', .slr[n])]] > .fldlvl
    }
  }

  result <- result %>%
    dplyr::select(- dplyr::contains('slr'))

  result <- result %>%
    dplyr::group_by(the_date) %>%
    dplyr::summarize(year = first(year),
                     dplyr::across(contains('exceeds'), any, na.rm = TRUE),
                     .groups = 'drop')

  count_years <- length(unique(result$year))
  start <- min(result$the_date)
  end <- max(result$the_date)

  result <- result %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(year = first(year),
                     days = n(),
                     dplyr::across(contains('exceeds'), sum),
                     .groups = 'drop') %>%
    dplyr::rename_with( ~ sub('exceeds', 'floods', .x))


  result <- result %>%
    # Calculate a value rescaled by the number of days of data
    mutate(dplyr::across(dplyr::contains('floods'), ~.x * 365.25 / days,
                           .names = "{.col}_p_yr")) %>%

    # Finally, calculate mean and standard deviations of yearly totals.
    dplyr::summarize(dplyr::across(dplyr::contains('floods'), c(mean = mean, sd = sd), na.rm = TRUE,
                                     .names = "{.col}_{.fn}"))

  result <- unname(unlist(result))

  # NOw, replace means we need to create row and column labels
  collabs <- c('Mean', 'SD')
  rowlabs <- c(paste0('floods_', .slr),
               paste0('floods_p_yr_', .slr))

  # Then we reshape the vector by specifying its dimensions
  dim(result) <- c(2, length(result)/ 2)
  # And transpose it, so we end up with two columns, not two rows.
  result = t(result)

  ## Apply the names
  rownames(result) <- rowlabs
  colnames(result) <- collabs

  # Add an attribute to retains the SLR values used.
  attr(result, 'slr') <- .slr
  attr(result, 'years') <- count_years
  attr(result, 'span') <- c(start = start, end = end)
  return(result)
}


# todo:  Consider whether to add a parameter to allow request to return the
# source data.  It's a waste to access the data from NOAA then just drop it.
# or alternatively, just don't offer this convenience function


#' @rdname floodcast_tub
#'
#' @details `floodcast_tub_lookup()` is a convenience function best suited for
#'          "quick and dirty" analysis.  It retrieves large amounts of data from
#'          the NOAA API (making it relatively slow) and then runs the
#'          calculation of future flood frequencies on that data before
#'          tosses all of the data away. It is generally going to be far better
#'          to download the data yourself, using `retrieve_data()`, and call
#'          `floodcast_tub()`. That way you have the data to look over
#'          independently, and if you want to calculate additional flood
#'          forecasts with different flood levels of sea level rise scenarios,
#'          you won't be waiting to download the data again.
#' @param .station  What NOAA water level station are you interested in?
#' @param .datum Character string, from a limited list of options. Specifies the
#'         vertical datum used to express the flood elevation. To ensure
#'         consistency, data will be downloaded, and analysis conducted in that
#'         datum.
#' @param .use_epoch Boolean.  Should analysis be based on the tidal epoch?
#'         Note that behavior may change on how this works with is this is set to "TRUE" for a
#' @param .start_yr If you don't base the analysis on the period of the epoch,
#'         what is the first year to use for the reference period?
#' @param .end_yr  If you don't base the analysis on the period of the epoch,
#'         what is the last year to use for the reference period?
#' @param .units Provide results in English (feet) or metric (meters) units?
#' @param .time_fmt
#' @export
#' @examples

floodcast_tub_lookup <- function(.station,
                                 .slr,
                                 .fldlvl,
                                 .datum = c('MSL', 'MTL',
                                            'MHW', 'MHHW',
                                            'MLLW', 'MLW',
                                            'NAVD', 'STND'),
                                 .wl_update = 0,
                                 .use_epoch = TRUE,
                                 .start_yr = NULL,
                                 .end_yr = NULL,
                                 .units = c('metric', 'english'),
                                 .timefmt = c('lst', 'gmt'))
{


  # Error checks on the function arguments

  # No parameter except .slr should have length != 1
  stopifnot(length(.station) == 1)
  stopifnot(length(.fldlvl) == 1)
  stopifnot(length(.start_yr) <= 1)
  stopifnot(length(.end_yr) <= 1)

  if (length(.use_epoch) != 1 || (! is.logical(.use_epoch))) {
    stop('The .use_epoch argument must be a single logical value.')
  }

  if(.use_epoch) {
    if (! is.null(.start_yr) && ! is.null(.end_yr))  {
      warning("start_yr and .stop_yr both specified while `.use_epoch = TRUE'.")
      warning("'.use_epoch' ignored.")
      .use_epoch = FALSE
    }
  }
  if(.use_epoch) {
    tryCatch(
      the_epoch <- get_epoch(.station),
      error=function(cond) {
        message(paste0('Unable to access epoch for station ', .station, '.'))
        message('Does the station exist?  Are you connected to the internet?')
        stop()
      }
    )
      .start_yr =  the_epoch[['start']]
      .end_yr   =  the_epoch[['end']]
  } else
    if (is.null(start_yr) || is.null(.end_yr)){
      stop("Must specify either '.use_epoch = TRUE' ",
           "or provide both .start_yr and .end_yr.")
    }

  .datum <- match.arg(.datum)
  .units <- match.arg(.units)
  .timefmt <- match.arg(.timefmt)


  tz <- ifelse(.timefmt == 'gmt', 'UTC', get_tz(.station, .type = 'string'))


  wl_data <- retrieve_data(.station, .start_yr, .end_yr,
                          .which = 'observed',
                          .datum =.datum,
                          .units = .units,
                          .timefmt = .timefmt,
                          .tz =  tz)
  floodcast <- floodcast_tub(wl_data, datetime, water_level,
                             .slr = .slr, .fldlvl = .fldlvl,
                             .wl_update = .wl_update)
  return(floodcast)

}

