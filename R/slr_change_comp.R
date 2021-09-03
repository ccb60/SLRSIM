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
#' This function compares the rate of sea level rise in the most recent period
#' (defined by `.span`) to the rate of sea level rise in prior periods of
#' the same length in the historic record.
#'
#' This function returns a probability that corresponds to the percentage of
#' prior evaluates periods with a slope equal to or greater than the most recent
#' slope.
#'
#' While conceptually, this is akin to a p-value, with the null hypothesis that
#' the current period is drawn from the same distribution as all prior slopes,
#' it should not be interpreted strictly. Consecutive slopes are strongly
#' auto-correlated. This analysis makes no effort to account for that
#' autocorrelation.
#'
#' @inheritParams slr_change
#' @param .interval Integer. Spacing of the start of time periods
#'        on which to calculate slopes. For `.mode == 'year'`, defaults to 1.
#'        Otherwise, must be provided.
#' @param .threshold Default = 0.75.  Real number specifying how strict the
#'        function should be in estimating slopes for periods with missing
#'        values. Slopes will only be estimated for time periods with at least
#'        `.threshold` times the number of non-missing data points in the time
#'        period examined that has the most data points. A `.threshold` of 1.0
#'        means any period with missing values will be excluded. A `.threshold`
#'        of 0 implies that all periods with at least two dates and water level
#'        values (so a slope can be estimated) will be retained.  Function
#'        output includes information on the number of samples on which each
#'        slope is based, so it is possible to set .threshold = 0 and filter the
#'        results intelligently.
#'
#' @return
#'
#' @family sea level rate functions
#'
#' @export
#'
#' @examples
#'
slr_change_comp = function(.data, .sl, .dt, .span = 20L,
                          .interval = if (.mode == 'year') 1L else NULL,
                          .mode = c('year', 'duration', 'count'),
                          .threshold = 0.75,
                          t_fit = FALSE) {

  # We have to figure out which argument was passed to .mode
  # BEFORE we access .interval, so .mode has length 1 before
  # R evaluates the default promise in the aruments list.
  .mode = match.arg(.mode)

  # Ugly argument checks, since they doesn't provide nice error messages.
  stopifnot(is.data.frame(.data) | is.null(.data))

  stopifnot(length(.span) == 1)
  stopifnot(is.numeric(.span) || inherits(.span, 'difftime'))

  stopifnot(length(.interval) == 1)
  stopifnot(is.numeric(.interval) || inherits(.interval, 'difftime'))

  stopifnot(length(.threshold) == 1 && is.numeric(.threshold))
  stopifnot(length(t_fit) == 1 && inherits(t_fit, 'logical'))

  sl_sym <- rlang::enquo(.sl)
  date_sym<- rlang::enquo(.dt)

  sl <- rlang::eval_tidy(sl_sym, .data)
  the_date <- rlang::eval_tidy(date_sym, .data)


  # Error Checks
  have_dates <- inherits(the_date, c('Date','POSIXt'))
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
           `(as.difftime(years * 365.2422, units = "days")`.')
    }
    else if(! inherits(.interval, 'difftime')) {
      stop('.mode == "duration" requires .interval to be of class "difftime".',
           'You can convert a number of years to (approximate) "difftime" with,
           `(as.difftime(years * 365.2422, units = "days")`.')
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
  if (.mode == 'year') {
    # pull years from the dates
    # TODO: add optional code to allow clipping to complete years
    years <- as.numeric(format(the_date, format = '%Y'))
    first_year <- min(years)
    last_year  <- max(years)

    # We want both the span and the interval to be rounded to whole years
    .span      <- round(.span, 0)  # subtract one so we can use full years
    .interval  <- round(.interval,0)

    # Create "ideal" intervals.  Not that we add one to the
    starts_ideal  <-  seq(first_year, last_year - (.span - 1), .interval)
    ends_ideal    <-  seq(first_year + (.span - 1), last_year , .interval)

    # But we want dates / times, not years
    # for each period we want to find the first and last date that matches.
    a_date <- as.Date('2021-07-15')
    starts <- vapply(starts_ideal, function(x) min(the_date[years>=x]), a_date,
                     USE.NAMES = FALSE)
    ends   <- vapply(ends_ideal, function(x) max(the_date[years<=x]), a_date,
                     USE.NAMES = FALSE)
    starts <- as.Date(starts, origin = as.Date('1970-01-01 '))
    ends <- as.Date(ends, origin = as.Date('1970-01-01 '))

    # We will want to label by years in this case.
    # There is probably a way to find this by indexing, and save the
    # format calls.
    label <- starts_ideal

    # Now we evaluate whether to include each interval in the results.
    # there should be a way to run this without the for loop....
    lengths <- numeric(length(starts))
    for (i in seq_along(starts))
          lengths[i] <- length(the_date[the_date >= starts[i] &
                                        the_date <= ends[i]])
    include <- lengths  >= .threshold * max(lengths)

    #browser()
    starts <- starts[include]
    ends   <- ends[include]
    label <- label[include]

  }

  else if (.mode == 'duration') {
    first_date <- min(the_date)
    last_date  <- max(the_date)

    # Dates and times may not line up perfectly
    # Create "ideal" intervals.
    starts_ideal <- seq(first_date,         last_date - .span, .interval)
    ends_ideal   <- seq(first_date + .span, last_date ,        .interval)

    # And then pull first and last dates in each period
    a_date <- as.Date('2021-07-15')
    starts <- vapply(starts_ideal, function(x) min(the_date[the_date>=x]), a_date,
                     USE.NAMES = FALSE)
    ends   <- vapply(ends_ideal, function(x) max(the_date[the_date<=x]), a_date,
                     USE.NAMES = FALSE)
    starts <- as.Date(starts, origin = as.Date('1970-01-01 '))
    ends <- as.Date(ends, origin = as.Date('1970-01-01 '))

    label <- starts

    # Now we evaluate whether to include each interval in the results.
    # there should be a way to run this without the for loop....
    lengths <- numeric(length(starts))
    for (i in seq_along(starts))
      lengths[i] <- length(the_date[the_date >= starts[i] &
                                      the_date <= ends[i]])
    include <- lengths  >= .threshold * max(lengths)

    #browser()
    starts <- starts[include]
    ends   <- ends[include]
    label <- label[include]

  }

  else if(.mode == 'count'){
    #browser()
    # we simply step through the record
    nsteps <- length(the_date) %/% .interval
    starts_intervals <-  rep(seq(1, nsteps), each = .interval)
    # We could have a few recent observations fall off the end of the intervals
    length(starts_intervals) <- length(the_date)
    starts_intervals[is.na(starts_intervals)] <- max(starts_intervals, na.rm = TRUE) + 1

    starts <- tapply(the_date, starts_intervals, min)
    starts <- as.Date(starts, origin = as.Date('1970-01-01 '))

    # we start by working only with sequence number in the data
    ends <- match(starts, the_date) + .span - 1
    # but some of our periods now run past the end of our data
    trouble <- ends > length(the_date)
    starts <- starts[! trouble]
    ends   <- ends  [! trouble]
    ends <- the_date[ends]

    label <- starts
  }

  samples <- numeric(length(starts))
  slopes  <- numeric(length(starts))
  pvals   <- logical(length(starts))

  for (index in seq_along(starts)) {
    dts <-  the_date[the_date >= starts[index] & the_date <= ends[index]]
    sls <-  sl[the_date >= starts[index] & the_date <= ends[index]]

    # TODO: Check that this form of `corAR1()` fits best-fit autocorrelations.
    if (t_fit) {
      mod <- nlme::gls(sls ~ dts, correlation = nlme::corAR1(0.75, form = ~dts))
    }
    else {
      mod <- nlme::gls(sls ~ dts, correlation = nlme::corAR1())
    }


    samples[index] <- sum(! is.na(dts) & ! is.na(sls))
    slopes[index]  <- coef(mod)[2]
    # TODO:  Add code for calculating the standard error of the slope estimates?
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
      message("Annual trends based on POSIXct times may be affected by ",
              "rounding. Consider rexpressing time coordinates as R Dates.")
      slopes <- slopes * 31556926
    }
    else if (inherits(the_date, 'POSIXt')) {
      warning("Don't know how to convert trend estimates based on time ",
      "values of class", class(the_date), "to an annual basis. Slopes are ",
      "expressed in original time units.")
    }
  }
  else {
    message("Trends not expressed on an annual basis. Consider converting ",
            "time coordinates to R Dates and setting .mode == 'year'.")
  }

  n_slopes <- length(slopes)
  n_higher <- sum(slopes[1:length(slopes)-1] >= slopes[length(slopes)])

  cor_struct = c('Order-based', 'Time-based')[t_fit + 1]

  results <- structure(list(n_higher = n_higher,
                            n_slopes = n_slopes,
                            df = data.frame(label, starts, ends, samples,
                                            slopes, pvals)),

                       mode = .mode,
                       span = .span,
                       interval = .interval,
                       cor_struct = cor_struct)
return(results)
}
