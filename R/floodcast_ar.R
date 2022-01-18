#'Fit Selected AR models
#'
#' This functions is just a thin wrapper around `forecast::Arima()`. The
#' function fits AR models that optionally include: linear trends by
#' YEAR,  seasonal AR terms, and annual fourier predictors to address seasonal
#' patterns.
#'
#' In initial model exploration for hourly tidal deviations data,
#' SARIMA(5,0,0)(1,0,0)25 models often performed well. Model performance could
#' be further improved by fitting low-order annual fourier terms. Since we know
#' that tidal deviations are non-stationary, it may also makes sense to fit a
#' linear predictor that can account for non-stationarity.
#'
#' Unfortunately, fitting complex models at this stage can complicate
#' integration of model simulations with the larger modeling strategy. So,
#' the function allows control of whether to fit different components. In
#' general, the user controls which components to fit by selecting whether or
#' not to include certain parameters or not. .
#'
#' These models tend to produce simulated deviations with temporal structure
#' that is similar to observed tidal deviations, but the distribution of
#' simulated deviations tends to be less skewed and less kurtotic
#' as actual tidal deviation data. Thus models are likely to underestimate the
#' frequency of extreme events.
#'
#' @param .data   A data frame or null
#' @param .dev Data column in .data or defined externally, to which the model
#'        will be fit. In our context, usually deviations between observed and
#'        predicted water levels.
#' @param .x  Data column in .data or defined externally that defines a single
#'        linear predictor.  In our setting, this is usually a term that
#'        increments by year, so that the model coefficient associated with this
#'        predictor provides an estimate of annual sea level rise.  If omitted,
#'        no linear predictor is included in the model.
#' @param .tidal_period How many observations are included in each seasonal
#'        period? For hourly observations, this is often approximately 25 hours.
#' @param .annual_period How many observations are included in each year?
#'        This value is used  to fit annual fourier term or terms. fourier term period? For hourly
#'        observations, this is approximately 325.25 (days per year) times 24
#'        (hours per day) = 8766 observations per year.
#' @param .order Small positive integer.  Default = 5.  The order of the autoregressive
#'        component of the model.
#' @param .tidal_order  Small positive integer.  Default = 1.  The order of the
#'        'seasonal' (usually tidal) autogregressive model.
#' @param .annual_terms  Small positive integer.  Default = 1.  This establishes how many
#'        fourier terms are fit to the annual pattern. A first order fourier
#'        fit (the default) fits a simple sine curve to the annual data to
#'        reflect seasonal patterns. Higher order fits allow more complex
#'        matches to the seasonal pattern. Given the high noise and relatively
#'        small  annual pattern in these data (most of the pattern was removed
#'        by NOAA when they construct the model predicting tides), a low order
#'        fit (1 or 2) is usually most appropriate.
#'
#' @return An ARIMA model, with `class = c("forecast_ARIMA", "ARIMA","Arima")`.
#'         this can be a large object (> 1.25 MB), but the full model object is
#'         needed to simulate from more complex ARIMA models, such as
#'         non-stationary models or models with predictors.
#' @export
#'
#' @examples
.ar_fit<- function(.data, .dev, .x,
                   .annual_period = 365.25*24, .tidal_period = 25,
                   .order = 5, .tidal_order = NULL, .annual_terms = NULL) {
  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dev <- rlang::enquo(.dev)
  dev <- rlang::eval_tidy(.dev,.data)
  the_ts <- ts(dev, frequency = .tidal_period)

  if (! missing(.x)) {
    .x <- rlang::enquo(.x)
    x <- rlang::eval_tidy(.x,.data)
  } else{
    x = NULL   # this works with Arima. it checks for NULL, not missing()
  }

  # Each parameter must be `length() = 1` and either numeric integer or NULL.
  # The order stats can't be NULL (but they can be zero).

  stopifnot(length(.annual_period) == 1 &&
              (is.null(.annual_period) ||
                is_whole(.annual_period)))
  stopifnot(length(.tidal_period) == 1 &&
              (is.null(.tidal_period) ||
                is_whole(.tidal_period)))
  stopifnot(length(.order) == 1 &&
              is_whole(.order))
  stopifnot(length(.tidal_order) == 1 &&
                is_whole(.tidal_order))
  stopifnot(length(.annual_terms) == 1 &&
              (is.null(.annual_terms)||
                is_whole(annual_terms)))

  the_ts <- msts(dev, seasonal.periods = c(.tidal_period, .annual_period),
                 ts.frequency = .tidal_period)

  xreg <- x
  # add code to add column names here somewhere!
  if (! is.null(.annual_terms)) {
    xreg = cbind(xreg, fourier(the_ts, K = c(0, .annual_terms)))
  }

  my_arima <- forecast::Arima(ts(dev, frequency = .tidal_period),
                           order = c(.order,0, 0),
                           seasonal = c(.tidal_order, 0, 0),
                           xreg = xreg)
  return(my_arima)
}

#' Generate a Single Simulation of Tidal Deviations
#'
#' Simulates a single random time series, based on an ARIMA model.  Can accept
#' regression parameters as well, usually Year and an annual (season) fourier
#' decomposition. terms
#'
#' For now, both .nonzero and .seasonal are needed to correctly interpret the `.coefs`
#' parameter. That may change as we revise this function, as the
#'
#' @inheritParams floodcast_tub
#' @param .predicted The (astronomical) tidal predictions. A numerical data
#'        column in .data or a numerical vector defined in the enclosing
#'        environment.
#' @param .model a time series model with a suitable `simulate()` method,
#'        usually an ARIMA model model of `class = c("ARIMA")`.
#' @param .len Desired length of simulated output. IF `NULL`, defaults to the
#'        length of the input deviations.
#'
#' @family Flood frequency analysis functions
#' @return numerical vector of length equal to input vector
#' @export
#'
#' @examples
#'
.sim_once <- function(.data, .dt, .predicted, .model,
                      .slr, .fldlvl, .wl_update = 0, .len = NULL) {

  # .data is a dataframe
  # .predicted is the data column containing the (astronomical) sea level
     predictions
  # .dt is a data column of identifiers by date / day
  # .slr is the value for SLR for estimating future tidal elevations
  # .fldlvl is the selected elevation for something to be considered a flood
  # coefs is a list of coefficients as produced by arima() or auto.arima()
  # sigma2 is the sigma2 measure of variation from arima() or  auto.arima()

  # Returns the mean number of floods per year over ONE simulated tidal epoch.

  # We quote data variables, and look them up by name
  # Caution:  there is no error checking.  if things are not working it may be
  # because the columns do not exist.
  .predicted <- as.character(ensym(.predicted))
  #dev <- as.character(ensym(dev))
  .dt <- as.character(ensym(.dt))

  #Simulate one tidal epoch of hourly tidal elevations
  val <- .data[[.predicted]] + .slr + arima.sim(n = length(.data[[.predicted]]),
                                        model = list(ar = coefs[1:5],
                                                     ma = coefs[6:10]),
                                        sd = sqrt(sigma2))

  #create a dataframe, for convenient calculations
  df <- tibble(theDate = .data[[.dt]], sim = val)

  #Calculate results
  res <- df %>%
    group_by(theDate)  %>%
    summarize(exceeded = any(sim > .fldlvl),
              .groups = 'drop') %>%
    summarize(days = sum(! is.na(exceeded)),
              floods = sum(exceeded, na.rm = TRUE),
              floods_p_yr = 365.25 * floods/days) %>%
    pull(floods_p_yr)

  return(res)
}


#' Forecasting future coastal flood frequency based on time series simulation
#'
#' Bootstrap estimates of future annual flood frequency by adding simulated
#' deviations to predicted tides.
#'
#' The function simulates "random" deviations from tidal predictions in a
#' manner that creates deviations with statistical properties similar to those
#' of the real deviations observed in the past. We bootstrap the simulations to
#' produce estimates and standard errors of future annual flood frequencies.
#'
#' The simulations are based on an ARMA process derived from historic data. An
#' ARMA process is a specific flavor of time series model. (The acronym
#' reflects the fact that the model includes both "autoregressive" and "moving
#' average" model components.)
#'
#' By definition, the observed deviations during the tidal epoch have
#' mean zero (or very close to zero). Deviations not during the tidal epoch may
#' have either positive or negative mean.
#'
#' Ideally, they would also be stationary,
#' but in practice they often are not.  As we explain elsewhere, the deviations
#' often have a small positive slope, as the tidal predictions did not take into
#' account sea level rise. The effect is small, and we chose to overlook it for
#' modeling purposes.
#'
#'
#'
#'
#' @inheritParams floodcast_tub

floodcast_ar <- function(.data, .dt, .wl, .slr, .fldlvl, .wl_offset = 0) {

  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)
  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(is.numeric(.slr) || missing(.slr))
  stopifnot(length(.fldlvl) == 1 && is.numeric(.fldlvl))
  stopifnot(length(.wl_offset) == 1 && is.numeric(.wl_offset))
  stopifnot(inherits(dt, 'POSIXct'))

  if (missing(.slr) || (length(.slr) == 1  && is.na(.slr[[1]]))) {
    # Without .slr scenarios, we fill in with the null SLR case
    .slr <- 0
  }
}
