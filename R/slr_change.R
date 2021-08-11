
#' Test whether the rate of sea level rise has changed
#'
#' Fit a piecewise linear model to test whether a recent change in
#' rate of sea level rise has occurred.
#'
#' A frequent question raised in analysis of recent sea level records is whether
#' the rate of sea level rise is increasing, as predicted by numerous climate
#' models over the years. This is a simple question that is rather more complex
#' to answer statistically than it at first appears.
#'
#' This function checks for a change is rate of sea level rise by fitting a
#' piecewise linear model and testing if a change in slope at a (user-defined)
#' breakpoint improves the statistical fit enough to matter.  A piecewise linear
#' model fits the data with (here) two linear segments that join at a breakpoint
#' or knot. The underlying model is a generalized linear model, with an AR1
#' autocorrelated error term.
#'
#' The AR1 process, an "autoregressive process of order 1", is a simple
#' assumption about correlation structures that reduces the risk of overstating
#' the precision of trend estimates. Actual tidal data series tend to have more
#' complex autocorrelation structure. If working with high frequency data,
#' short-term structure should be removed before using this function, typically
#' by calculating average sea level over convenient and meaningful time periods,
#' like months or years.  See additional comments on use of the AR1 model in
#' analysis of sea level data in the help file for `slr_slope()`.
#'
#' This function is ripe for abuse. The problem is that rates of sea level
#' rise year over year vary.  By chance, some periods of the historic record
#' will have higher rates of sea level rise than others.  This makes it far too
#' easy to "cherry pick" starting dates for a recent period of record, and claim
#' a statistically meaningful change in rate of sea level rise.  Use with
#' appropriate humility.
#'
#' For a more robust approach (with lower statistical power, but deeper
#' justification), see the function `all_sr_change()`
#'
#' @inheritParams slr_slope
#' @param .span  Span defining the "recent" period for fitting the model.
#'        integer or difftime object.  Interpretation of .span depends on the
#'        value of the by_year. If `by_year == TRUE`, `.span` must be an
#'        integer, and is interpreted as the number of years included in the
#'        "recent" period for fitting the model.  If `by_year == FALSE`, and
#'        `.span` is an integer, it is interpreted as the number of records to
#'        include in the 'recent' period.
#' @param .mode one of c('year', 'duration', 'count') indicating whether the .span
#'         is expressed in years, time coordinates, or number of observations.
#'         If `.mode == 'year'`, slopes are scaled to be expressed on a per year
#'         basis.  Scaling requires the time coordinate to inherit from R
#'         `Date` or `POSIXt` objects.  Rates based on `Dates` are converted to
#'         annual values by multiplying by 365.2422 days / year.  Rates based on
#'         `POSIXt` objects are converted to annual values by multiplying slope
#'         estimates by multiplying by 31556926 seconds / year (with a warning
#'         about rounding error). No scaling is done for `.mode == 'duration'` or
#'         `.mode == 'count'`.
#' @param retain_model  Boolean.  If `TRUE`, the piecewise linear model is
#'        returned via the 'model' slot of the return value.
#'
#' @return a list of three (optionally four) items, with the following
#' components:
#'
#' \describe{__summary__ (Vector of numerical values.)
#' \item{slope_ratio}{The ratio of the slope for the "recent" period to
#'       the slope for the remainder of the historic record.}
#' \item{slope_old}{The estimated slope of changes in sea level prior to the
#'       "recent" period.}
#' \item{slope_old_err}{Standard error of `slope_old`.}
#' \item{slope_recent}{Estimated slope during the "Recent" period.}
#' \item{slope_recent_err}{Standard error of `slope_recent`.}
#' }
#'
#' \describe{__details__  (Vector of numerical values.)
#' \item{l_ratio}{likelihood ratio comparing the piecewise linear model to a
#'       linear model.}
#' \item{p-value}{the p-value (by ANOVA) comparing the two models.}
#' \item{delta-AIC}{Change in AIC between the two models.  A value less than
#'       about -2 is evidence that the piecewise model would outperform the
#'       strict linear model for "predicting" past sea levels.}
#' \item{sample}{Sample size on which model is based, including both older and
#'       recent data.}
#' \item{recents}{the number of samples in the "recent" time period.}
#' }
#'
#' \describe{__settings__ (Vector of strings.)
#' \item{mode}{The value of the `.mode` argument specifying how to process
#'       the data.}
#' \item{span}{The value of the `.span` argument, as a string, not a number. If
#'       `.mode = 'year'`, the default, this will be the number of years in the
#'       'recent' period. Otherwise, it will represent either a time period,
#'       (`.mode = 'duration'`) or a number of samples (`.mode = 'count'`).}
#' \item{cor_struct}{Either 'Order-based' (for `t_fit = FALSE`) or 'Time-based',
#'       for `t_fit = TRUE`.}
#' }
#'
#' \describe{__model__
#' The underlying piecewise GLS model, returned only if
#'       `retain_model = TRUE`}
#'
#' @family sea level rate functions
#'
#' @export
#'
#' @examples
#'
slr_change <- function(.data, .sl, .dt, .span = 20L,
                     .mode = c('year', 'duration', 'count'),
                     t_fit = FALSE,
                     retain_model = FALSE) {

  # Ugly argument checks, since they doesn't provide nice error messages.
  stopifnot(is.data.frame(.data) | is.null(.data))
  stopifnot(length(retain_model) == 1 && inherits(retain_model, 'logical'))
  stopifnot(length(.span) == 1)
  stopifnot(is.numeric(.span) || inherits(.span, 'difftime'))
  stopifnot(length(t_fit) == 1 && inherits(t_fit, 'logical'))

  sl_sym <- rlang::ensym(.sl)
  date_sym<- rlang::ensym(.dt)

  sl <- rlang::eval_tidy(sl_sym, .data)
  the_date <- rlang::eval_tidy(date_sym, .data)

  .mode = match.arg(.mode)

  have_dates <- inherits(the_date, 'Date') || inherits(the_date, 'POSIXct')
  have_diff <-  inherits(.span, 'difftime')

  # # Error Checks
  # if(.mode == 'year')
  #   if( ! have_dates) {
  #     stop('.mode == "year" requires .dt to be a Date or POSIX time.\n',
  #          'You can convert integer years to dates with,
  #          `as.Date(paste(year, "06", "15"), sep = "-")`')
  #   }
  # if(abs(.span - as.integer(.span)) > 0.001) {
  #   stop('.mode == "year" requires .span to be an integer signifying the ',
  #        "number of years in the 'recent' period.")
  # }
  # else if (mode == 'duration') {
  # if ( ! inherits(.span, 'difftime')) {
  #   stop(".mode = 'duration' requires .span to be an R  `difftime` object." )
  # }
  #
  # }
  # else if (mode == 'count') {
  #   if(abs(.span - as.integer(.span)) > 0.001) {
  #     stop('.mode == "count" requires .span to be an integer signifying the ',
  #          "number of observations in the 'recent' period.")
  #   }
  # }

  # Remember that `is.integer()` checks for the storage mode, not whether the
  # parameter passed is a LOGICAL integer, which can be stored in a double.
  # We check for integer values by difference.  The tolerance here is one in
  # one thousand, which is below one day out of the year, but above the accuracy
  # of double precision values.
  if(.mode == 'year') {
    if (! have_dates) {
      stop('.mode == "year" requires .dt to be a Date or POSIXct time.\n',
           'You can convert integer years to dates with,
           `as.Date(paste(year, "06", "15"), sep = "-")`')
    }
    else if(! abs(.span - as.integer(.span)) < 0.001) {
      stop('If .mode == "year",  .span must be an integer giving the number',
           'of years over which to calculate the recent sea level trend.')
    }
  }

  else if (.mode == 'duration') {
    if (! inherits(.span, 'difftime')) {
      stop('If .mode == "time", .span must be a difftime object.')
    }
    if(! inherits(the_date, c('Date', 'POSIXct'))) {
      stop('If .mode == "time", .dt must be either a Date or POSIXct object.')
    }
  }
  else if (.mode == 'count') {
    if(! abs(.span - as.integer(.span)) < 0.001) {
      stop('If .mode == "year",  .span must be an integer giving the number',
           'of observations over which to calculate the recent sea level trend.')
    }
  }

  # Reorder the data by the time stamp.  Only essential for .mode == 'count',
  # but it costs little otherwise.
  sl <- sl[order(the_date)]
  the_date <- the_date[order(the_date)]

  # Actually calculate the break point.  We have three cases.
  if (.mode == 'year') {
    last_year <- as.numeric(format(max(the_date), format = '%Y'))
    cutyear <- last_year - .span
    # Move to EOY, since we also include the most recent year.
    # TODO:  add code to allow automated trimming of partial years.
    cutdate <- as.Date(paste0(cutyear, '-12-31'), format = '%Y-%m-%d')
    is_recent <- the_date > cutdate
  }
  else if (.mode == 'duration') {
    last_date <- max(the_date)
    cutdate <- last_date - .span
    is_recent <- the_date >= cutdate
  }
  else if (.mode == 'count') {
    is_recent <- logical(length(the_date))
    cutpoint <- length(the_date) - round(.span, 0)
    is_recent[1: cutpoint ] <- FALSE
    is_recent[(cutpoint + 1):length(the_date)] <- TRUE
    cutdate <- min(the_date[is_recent])
    message('Recent data includes the most recent ', .span, ' observations.')
  }
  else {
    stop(".mode ", .mode, "not recognized. Should be one of ",
         "c('year', 'duration', 'count')")
  }
  message('The first date in the recent period is ', min(the_date[is_recent]))
  message('The last date in the recent period is ', max(the_date[is_recent]))


  # Create date-time difference
  # We will fit a (linear) parameter to this variable, which enables
  # a modified  slope after cutdate.
  recent_dt <- dplyr::if_else(is_recent, the_date - cutdate, 0)

  # Run the Actual Models.  We run the_gls to allow likelihood tests and compare
  # model AICs.
  the_gls <- nlme::gls(sl~the_date,
                 correlation = nlme::corAR1(), method = 'ML')

  piecewise_gls <- nlme::gls(sl ~ the_date + recent_dt,
                       correlation = nlme::corAR1(), method = 'ML')

  #TODO:  Add code for t_fir = TRUE
  the_anova <- anova(the_gls, piecewise_gls)


  # For .mode == 'year', we convert units for Date or POSIXct objects
  if (.mode == 'year'&& inherits(the_date, 'Date')) {
    multiplier <- 365.2422
  }
  else if (.mode == 'year' && inherits(the_date, 'POSIXct')) {
    # 365 days, 5 hours, 48 minutes, and 46 seconds
    multiplier <- 31556926  #seconds per year, on average
    message("Annual trends based on POSIXct times may be affected by ",
            "rounding. Consider rexpressing time coordinates as R Dates.")
  }
  else {
    multiplier <- 1
  }

  # Calculate "upper" slope and its standard error.
  betas <- coef(piecewise_gls)
  vcv <- vcov(piecewise_gls)
  slope_recent <- sum(betas[2:3])
  slope_recent_err <- sqrt(sum(vcv[2:3,2:3]))

  cor_struct = c('Order-based', 'Time-based')[t_fit + 1]
  #browser()
  r <- c (slope_old = betas[[2]] * multiplier,
          slope_old_err = sqrt(vcv[[2,2]]) * multiplier,
          slope_recent = slope_recent * multiplier,
          slope_recent_err = slope_recent_err * multiplier)
  r['slope_ratio'] <- r['slope_recent']/r['slope_old']
  r <-r[c(5,1:4)]

  details <- c(l_ratio = the_anova$L.Ratio[[2]],
               p_value = the_anova$`p-value`[[2]],
               delta_AIC = the_anova$AIC[[2]] - the_anova$AIC[[1]],
               sample = sum(! (is.na(sl) | is.na(the_date))),
               recents = sum(is_recent, na.rm = TRUE))
  settings <- c(mode = .mode,
               span = .span,
               cor_struct = cor_struct)

  results <- list(summary = r, details = details, settings = settings)
  if(retain_model) {
    results[['model']] <- piecewise_gls
  }

  return(results)
}

