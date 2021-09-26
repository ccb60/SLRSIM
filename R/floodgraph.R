
#todo consider adding parameters to allow other widths for the error bands.

#' Generate a Graphic showing Historical Flood Frequencies or Probabilities
#'
#' This function automated making a simple historical graph of the frequency
#' of flooding in prior years.  It provides only limited options for
#' customization
#'
#' @inheritParams floodcounts
#' @param .type.  Enumerated choices.  Possible values include
#'        'count', and 'adjusted'. If 'count', the plot will show the observed
#'        number of flood events in each year.  If 'adjusted', the value shows
#'        an estimate of how many flood events WOULD have happened if data for
#'        that year were complete.  The adjusted value assumes risk of flooding
#'        is independent of whether data is available or not, which may be an
#'        unreasonable assumption, especially if flood risk is strongly
#'        seasonal. In practice, most NOAA stations have nearly complete data
#'        for most years, so the choice makes little difference.
#' @param .fit `TRUE`/`FALSE`.  Should a binomial GLM trendline be added to the
#'        plot?  The trendline is produced from the parameters of a binomial
#'        GLM, with logit link, which estimates (possibly time-varying" )
#' @param .se `TRUE`/`FALSE`.  Should the plot show error bands showing +/- one
#'        standard error?
#' @param .dotcol  Color specification for points on the figure
#' @param .linecol Color specification for the optional trendline
#' @param .bandcol color specification for the optional error band
#'
#' @family Flood frequency analysis functions
#' @return A list, the first item of whihc is a `ggplot` object.  The plot has
#'         years across the x axis, and the number of flood events along the Y
#'         axis.  Optionally, including a trendline.
#'         If `.fit = TRUE`, the list will include  second item that
#'         provides summary information about the model fit.
#' @export
#'
#' @examples
floodgraph <- function(.data, .dt, .wl, .fldlvl,
                       .type = c('count', 'adjusted'),
                       .fit = FALSE,
                       .se = FALSE,
                       .dotcol = 'slateblue',
                       .linecol = 'black',
                       .bandcol = scales::muted(.dotcol)) {

  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)

  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(length(.fldlvl) == 1 && is.numeric(.fldlvl))
  stopifnot(length(.fit) == 1 && is.logical(.fit))
  stopifnot(length(.se) == 1 && is.logical(.se))

  stopifnot(inherits(dt, 'POSIXct'))

  type <- match.arg(.type)
  # create a dataframe, for convenience use of the
  #browser()
  response <- dplyr::tibble(theDT = dt, obs_wl = wl) %>%
    dplyr::mutate(theDate = as.Date(theDT))

  response <- response %>%
    dplyr::group_by(theDate)  %>%
    dplyr::summarize(theYear = first(as.numeric(format(theDate, format = '%Y'))),
                     exceeded = any(obs_wl > .fldlvl),
                     .groups = 'drop')

  response <- response %>%
    dplyr::group_by(theYear) %>%
    dplyr::summarize(days = sum(! is.na(exceeded)),
                     flood_days = sum(exceeded, na.rm = TRUE),
                     not_flood_days = days - flood_days,
                     daily_p_flood = flood_days/days,
                     adjusted = 365.25 * daily_p_flood,
                     .groups = 'drop')

  # We want the trendline, and especially any error band, to lie behind the
  # points, so it is convenient to start with an "empty" chart and build it
  # up part by part.

  if(type == 'count') {
    yval <- response$flood_days
  } else if(type == 'adjusted') {
    yval = response$adjusted
  } else {
    stop("Unrecognized '.type' parameter.")
  }

  plt <- ggplot(response, aes(theYear, yval))
  if (.fit) {
    #browser()
    mod <- glm(cbind(response$flood_days, response$not_flood_days) ~ response$theYear,
               #data = response,
               family = binomial(link = 'logit'))
    preds <- predict(mod, newdata = list(theYear =
                                   min(response$theYear):max(response$theYear)),
                     type = 'response',
                     se.fit = TRUE)   # even if we don't use fit.se, this
                                      # ensures format of the returned value
    preds$fit <- 365.25 * preds$fit
    preds$se.fit <- 365.25 * preds$se.fit
    if(.se){
      plt <- plt + geom_ribbon(aes(x = theYear,
                                   ymin = preds$fit - preds$se.fit,
                                   ymax = preds$fit + preds$se.fit ),
                               fill = .bandcol,
                               alpha = 0.2)
    }
    plt <- plt + geom_line(aes(y = preds$fit), color = .linecol)
  }
  plt <- plt + geom_point(color = .dotcol) +
    ylab('Days with Coastal Flooding') +
    xlab('Year')

  if(.fit) {
    coefs <- coef(mod)
    intercept <- coefs[1]
    slope <- coefs[2]
    vars <- diag(vcov(mod))
    intercept_se <- sqrt(vars[1])
    slope_se <- sqrt(vars[2])
    p <- coef(summary(mod))[2,4]

    return(structure(plt, flood_fit = c(intercept = intercept,
                                    intercept_se = intercept_se,
                                    slope <- slope,
                                    slope_se <- slope_se,
                                    n = sum(! is.na(response$flood_days)),
                                    p = p)))
  } else {
    return(plt)
  }
}
