#' Place recent rate of sea level change into historical context
#'
#' Compares the recent rate of sea level rise to rates for all prior periods of
#' the same length.
#'
#' A frequent question raised in analysis of recent sea level records is whether
#' the rate of sea level rise is increasing, as predicted by numerous climate
#' models over the years. This is a simple question that is rather more complex
#' to answer statistically than it at first appears.
#'
#' This function checks for a change in rate of sea level rise in the most
#' recent period of .span years by comparing it to every prior period of the
#' same length in the historic record.
#'
#' This function returns a probability that corresponds to the percentage of
#' all prior periods with a slope equal to or greater than the most recent
#' slope.
#'
#' While conceptually, this is akin to a p-value, with the null hypothesis that
#' the current period is drawn from the same distribution as all prior slopes,
#' it should not be interpreted strictly. Consecutive slopes are strongly
#' auto-correlated, and this analysis makes no effort to account for that.
#'
#' @param .data Source data frame for data.  Use NULL if no data frame is used
#'        and all data is passed from the enclosing environment.
#' @param .sl  The data variable (usually found in the data frame) showing sea
#'        level or mean sea level.  Must be a named variable, not an
#'        expression.
#' @param .dt   Data variable containing corresponding midpoint dates for the
#'        period of averaging used to calculate .sl. Must be a named variable,
#'        not an expression. Midpoint dates for a given month can be
#'        approximated with `as.Date(paste(year, month, 15, sep = '-')`.
#' @param .span.  Span defining the "recent" period for fitting the model.
#'        integer or difftime object.  Interpretation of .span depends on the
#'        value of the by_year. If `by_year == TRUE`, `.span` must be an
#'        integer, and is interpreted as the number of years included in the
#'        "recent" period for fitting the model.  If `by_year == FALSE`, and
#'        `.span` is an integer, it is interpreted as the number of records to
#'        include in the 'recent' period.
#' @param .interval Integer.  Default = 1.  Spacing of samples on which to
#'        calculate slopes.
#' @param .mode one of c('year', 'duration', 'count') indicating whether the results should be scaled to
#'        annual values by multiplying by 365.25. If `.dt` is not a Date, this
#'         is ignored, and no scaling is conducted.
#' @param t_fit Should models be fit based on time coordinates, or
#'        only on the sequence of observations in the data?  Setting this
#'        to TRUE is safer if you are uncertain of the sequence of observations
#'        in the source data, or if there are many missing values in your data,
#'        but it slows model fitting somewhat.
#' @return
#'
#' @family sea level rate functions
#'
#' @export
#'
#' @examples
#'
all_slr_change = function(.data, .sl, .dt, .span = 20L, .interval = 1L,
                          .mode = c('year', 'duration', 'count'),
                          t_fit = FALSE) {

  # Ugly argument checks, since they doesn't provide nice error messages.
  stopifnot(is.data.frame(.data) | is.null(.data))
  stopifnot(inherits(by_year, 'logical'))
  stopifnot(length(by_year) == 1)
  stopifnot(is.numeric(.interval))
  stopifnot(length(.interval) == 1)
  stopifnot(inherits(t_fit, 'logical'))
  stopifnot(length(t_fit == 1))

  sl_sym <- rlang::ensym(.sl)
  date_sym<- rlang::ensym(.dt)

  sl <- rlang::eval_tidy(sl_sym, .data)
  the_date <- rlang::eval_tidy(date_sym, .data)

  .mode = match.arg(.mode)

  # Error Checks
  have_dates <- inherits(the_date, 'Date') || inherits(the_date, 'POSIXt')
  # Remember that `is.integer()` checks for the storage mode, not whether the
  # parameter passed is a LOGICAL integer, which can be stored in a double.
  # We check for integer values by difference.  The tolerance here is one in
  # one thousand, which is below one day out of the year, but above the accuracy
  # of double precision values.
  if(.mode == 'year') {
    if (! have_dates) {
      stop('.mode == "year" requires .dt to be a Date or POSIX time.\n',
           'You can convert integer years to dates with,
           `as.Date(paste(year, "06", "15"), sep = "-")`')
    }
    else if(! abs(.span - as.integer(.span)) < 0.001) {
      stop('.mode == "year" requires integer-valued .span giving',
           'the number of years over which to calculate slopes.')

    }
    else if(! abs(.interval - as.integer(.interval)) < 0.001) {
      stop('.mode == "year" requires integer-valued .interval giving',
           'the number of years between slope estimates (defaults to 1).')
    }
  }

  else if(.mode == 'duration') {
    if (! have_dates) {
      stop('.mode == "duration" requires .dt to be a Date or POSIX time.\n',
           'You can convert integer years to dates with,
           `as.Date(paste(year, "06", "15"), sep = "-")`')
    }
    else if(! inherits(.span, 'difftime')) {
      stop('.mode == "duration" requires .span to be of class "difftime".',
           'You can convert a number of years to (approximate) "difftime" with,
           `(as.difftime(years * 365.25, units = "days")`.')
    }
    else if(! inherits(.interval, 'difftime')) {
      stop('.mode == "duration" requires .interval to be of class "difftime".',
           'You can convert a number of years to (approximate) "difftime" with,
           `(as.difftime(years * 365.25, units = "days")`.')
    }

  }

  else if (.mode == 'count') {
    if (! abs(.span - as.integer(.span)) < 0.001) {
      stop('.mode == "count" requires integer-valued .span giving',
           'the number of observations over which to calculate slopes.')
    }
    else if (! abs(.interval - as.integer(.interval)) < 0.001) {
      stop('.mode == "count" requires integer-valued .interval giving',
           'the number of observations between origins for calculate slopes.')
    }
  }

  # Reorder the data by the time stamp.  Only essential for mode = 'count',
  # but does little harm otherwise.
  sl <- sl[order(the_date)]
  the_date <- the_date[order(the_date)]

  # Calculate the slopes.  We have a case for each mode.
  #  Because the intervals can be defined three ways, it is convenient to use
  # `findInterval() to isolate case-specific code from common code.
  if (.mode == 'year') {
    # pull years from the dates
    # todo: add optional code to allow clipping to complete years
    years <- as.numeric(format(the_date, format = '%Y'))
    first_year <- min(years)
    last_year  <- max(years)

    # We want both the span and the interval to be rounded to whole years
    .span      <- round(.span,0)
    .interval  <- round(.interval,0)
    # Create "ideal" intervals
    starts_ideal  <-  seq(first_year, last_year - .span, .interval)
    # pull actual start and end dates
    start_intervals <- findinterval(years, starts_ideal)

    # identify start and end dates for each interval
    starts <- tapply(the_date, starts_intervals, min)
    ends <- tapply(the_date, starts_intervals, max)
    # We will want to label by years in this case.
    label <- starts_ideal
  }

  else if (.mode == 'duration') {
    first_date <- min(the_date)
    last_date  <- max(the_date)

    # Dates and times may not line up perfectly, so we grab the first and
    # last datetime within each designated range.
    starts_ideal <- seq(first_date, last_date - .span, .interval)
    start_intervals <- findinterval(the_date, starts_ideal)

    starts <- tapply(the_date, starts_intervals, min)
    ends <- tapply(the_date, starts_intervals, max)
    # We label with the start date
    label <- starts
  }

  else if(mode == 'count'){
    # we simply step through the record
    nsteps <- length(the_date) %/% .interval
    starts_intervals <-  rep(se(1, nsteps), each = .interval)
    # We could have a few recent observations fall off the end of the intervals
    length(starts_intervals) <- length(the_date)
    starts_intervals[is.na(starts_intervals)] <- max(starts_intervals) + 1

    starts <- tapply(the_date, starts_intervals, min)
    ends <- tapply(the_date, starts_intervals, max)
    # We label with the start date
    label <- starts
  }

  slopes  = numeric(length(starts))
  pvals = logical(length(starts))
  for (index in seq_along(starts)) {
    message(paste(starts[index], ends[index], sep = ', '), '\n')
    dts <-  the_date[the_date >= starts[index] & the_date <= ends[index]]
    sls <-  sl[the_date >= starts[index] & the_date <= ends[index]]

    # TODO: Check that this form of `corAR1()` fits best-fit autocorrelations.
    if (t_fit) {
      mod <- nlme::gls(sls ~ dts,
                           correlation = nlme::corAR1(0.75, form = ~dts))
    }
    else {
      the_gls <- nlme::gls(sls ~ dts,
                           correlation = nlme::corAR1())
    }


    mod <-  gls(sls ~ dts, correlation = corAR1(), method = 'REML')

    # Does the following pull all the stats?
    samples[index] <- sum(! is.na(dts) & ! is.na(sls))
    slopes[index]  <- coef(mod)[2]
    pvals[index] <- summary(mod)$tTable[2,4]
  }

  if (.mode == "year") {
    # we need to adjust slope estimate to annual values
    # The primary challenge here is that we need to
    # figure out whether the data passed to us was in dates or times or nothing
    # dates are integers by day.  POSIXct times are integers by second.

    # The current length of a year is
    # 365 days, 5 hours, 48 minutes, and 46 seconds
    # 365.2422 days
    # 31556926 seconds.

    if (inherits(the_date, 'Date')) {
      slopes <- slopes * 365.2422
    }
    else if (inherits(the_date, 'POSIXct')) {
      message("Annual trends based on POSIXct times may be affected by",
              "rounding. Consider rexpressing time coordinates as R Dates.")
      slopes <- slopes * 31556926
    }
    else if (inherits(the_date, 'POSIXt')) {
      warning("Don't know how to convert trend estimates based on time",
      "values of class", class(the_date), "to an annual basis. Slopes are",
      "expressed in original time units.")
    }
  }
  else {
    message("Trends not expressed on an annual basis. Consider converting",
            "time coordinates to R Dates and setting .mode == 'year'.")
  }

  n_slopes <- length(slopes)
  n_higher <- sum(slopes[1:length(slopes)-1] >= slopes[length(slopes)])
  # Consider:  should this be just divided by n-slopes?
  # do we want to express probability that a prior slope was as high or higher
  # than the most recent slope, or the probability that any slope is as
  # high or higher than the most recent slope?
  p_higher <- n_higher / (n_slopes - 1)

  cor_struct = c('Order-based', 'Time-based')[t_fit + 1]

  results <- structure(list(p_higher = p_higher,
                            n_higher = n_higher,
                            n_slopes = n_slopes,
                            df = data.frame(label, starts, stops, samples,
                                            slopes, pvals)),
                       mode = .mode,
                       interval = .interval,
                       cor_struct = cor_struct)
return(results)
}
