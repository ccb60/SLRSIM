
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
#' @param .mode one of c('year', 'time', 'count') indicating whether the .span
#'         is expressed in years, time coordinates, or number of observations.
#'         If `.mode == 'year'`, slopes are scaled to be expressed on a per year
#'         basis.  Scaling requires the time coordinate to inherit from R
#'         `Date` or `POSIXt` objects.  Rates based on `Dates` are converted to
#'         annual values by multiplying by 365.2422 days / year.  Rates based on
#'         `POSIXt` objects are converted to annual values by multiplying slope
#'         estimates by multiplying by 31556926 seconds / year (with a warning
#'         about rounding error). No scaling is done for `.mode == 'time'` or
#'         `.mode == 'count'`.
#' @param retain_model  Boolean.  If `TRUE`, the piecewise linear model is
#'        returned via the 'model' slot of the return value.
#'
#' @return a list, with the following components:
#' \describe{
#' \item{Sample}{Sample size on which model is based}
#' \item{L.Ratio}{likelihood ratio comparing the piecewise linear model to a
#'       simple linear model.}
#' \item{p-value}{the p-value (by ANOVA) comparing the two models.}
#' \item{delta-AIC}{Change in AIC between the two models.  A value less than
#'       about -2 is evidence that the piecewise model would outperform the
#'        strict linear model for "predicting" past sea levels.}
#' \item{slope_old}{The estimated slope of changes in sea level prior to the
#'       "recent" period specified via the .span parameter.}
#' \item{slope_old_err}{Standard error of that slope.}
#' \item{slope_recent}{Estimated slope during the "Recent" period,  This is
#'       calculated as the sum of two model parameters, the overall slope and
#'       a "recent" correction.}
#' \item{slope_recent_err}{Standard error of the recent slope.  This is
#'       calculated using the formula for the variance for the sum of two
#'       uncorrelated random variables, treating the overall slope and
#'       recent slope correction as uncorrelated.}
#' \item{model}{The underlying piecewise GLS model, returned only if
#'       retain_model == TRUE}
#' }
#'
#' @family sea level rate functions
#' @export
#'
#' @examples
#'
slr_change = function(.data, .sl, .dt, .span = 20L,
                     .mode = c('year', 'duration', 'count'),
                     t_fit = FALSE,
                     retain_model = FALSE) {

  # Ugly argument checks, since they doesn't provide nice error messages.
  stopifnot(is.data.frame(.data) | is.null(.data))
  stopifnot(inherits(retain_model, 'logical'))
  stopifnot(length(retain_model) == 1)

  sl_sym <- rlang::ensym(.sl)
  date_sym<- rlang::ensym(.dt)

  sl <- rlang::eval_tidy(sl_sym, .data)
  the_date <- rlang::eval_tidy(date_sym, .data)

  .mode = match.arg(.mode)

  # Error Checks
  # If `by_year == TRUE` then we find a breakpoint
  # at the beginning of the year identified by .span, otherwise, if
  # by_year == FALSE, we check for a difftime object, and otherwise use
  # the .span as a (negative) index into the data.

  # If we got dates, or times, we can work directly with years, otherwise, we
  # can't.

  have_dates <- inherits(the_date, 'Date') || inherits(the_date, 'POSIXt')
  have_diff <- ! by_year & inherits(.span, 'difftime')

  # Remember that `is.integer()` checks for the storage mode, not whether the
  # parameter passed is a LOGICAL integer, which can be stored in a double.
  # We check for integer values by difference.  The tolerance here is one in
  # one thousand, which is below one day out of the year, but above the accuracy
  # of double precision values.
  if(by_year) {
    if (! have_dates) {
    stop('by_year == TRUE requires .dt to be a Date or POSIX time.')
    }
    else if(! abs(.span - as.integer(.span)) < 0.001) {
      stop('If by_year == TRUE,  .span must be an integer giving the number of years.')
    }
  }

  if(! by_year) {
    if (! abs(.span - as.integer(.span)) < 0.001 || inherits(.span, 'difftime')) {
      stop('If by_year == FALSE,  .span must be a  a difftime object, or',
           'an integer giving a number of observations.')
    }
    if(have_diff && ! inherits(the_date, c('Date', 'POSIXt'))) {
      stop('If .span is given as a difftime object, .dt must be either a',
           'Date or POSIXt datetime object.')
    }
  }

  # Reorder the data by the time stamp.  Only essential for by_year == FALSE and
  # integer valued .span, but does little harm otherwise.
  sl <- sl[order(the_date)]
  the_date <- the_date[order(the_date)]

  # Actually calculate the break point.  We have three cases:  by_year == TRUE,
  # by_year == FALSE with difftime, or by_year == FALSE with integer.
  if (by_year) {
    last_year <- as.numeric(format(max(the_date), format = '%Y'))
    cutyear <- last_year - .span
    cutdate <- as.Date(paste0(cutyear, '-12-31'), format = '%Y-%m-%d')
    is_recent <- the_date > cutdate
  }
  else if (have_diff) {
    last_date <- max(the_date)
    cutdate <- last_date - .span
    is_recent <- the_date > cutdate
    message('Recent data includes data after ', cutdate)
  }
  else{
    is_recent <- logical(length(the_date))
    cutpoint <- length(the_date) - round(.span, 0)
    is_recent[1: cutpoint-1] <- FALSE
    is_recent[cutpoint:length(the_date)] <- TRUE
    cutdate <- max(the_date[! is_recent])
    message('Recent data includes the most recent', .span, ' observations.' )
  }

  message('Recent data includes data after ', cutdate)

  # Create date-time difference
  # We will fit a (linear) parameter to this variable, which enables
  # a modified  slope after cutdate.
  recent_dt = if_else(is_recent, the_date - cutdate, 0)

  # Run the Actual Models.  We run the_gls to allow likelihood tests and compare
  # model AICs.
  the_gls <- nlme::gls(sl~the_date,
                 correlation = corAR1(), method = 'ML')

  piecewise_gls <- nlme::gls(sl ~ the_date + recent_dt,
                       correlation = corAR1(), method = 'ML')

  the_sum <- summary(piecewise_gls)
  the_anova <- anova(the_gls, piecewise_gls)

  results <- list()
  results['Sample']  <- the_sum$dims$N
  results['L.Ratio'] <- the_anova$L.Ratio[[2]]
  results['p-value'] <- the_anova$`p-value`[[2]]
  results['delta_AIC'] <- the_anova$AIC[[2]] - the_anova$AIC[[1]]
  results['slope_old'] <- the_sum$tTable[[2,1]]
  results['slope_old_err'] <- the_sum$tTable[[2,2]]
  results['slope_recent'] <- the_sum$tTable[[2,1]] +the_sum$tTable[[3,1]]
  results['slope_recent_err'] <- sqrt(the_sum$tTable[[2,2]]^2 +
                                       the_sum$tTable[[3,2]]^2)
  results['slope_ratio'] = results$slope_recent/results$slope_old

  #browser()
  if(retain_model) {
    results[['model']] <- piecewise_gls
  }
  return(results)
}

