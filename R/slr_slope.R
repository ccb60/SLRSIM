


#' Calculate Long Term Estimate of SLR
#'
#' @param .data Source data frame
#' @param .msl  Data variable showing sea level or mean sea level
#' @param .dt   Data variable containing corresponding midpoint dates of the
#'        period of averaging for .msl.
#'
#' @return
#' @export
#'
#' @examples
slr_slope <- function(.data, .msl, .dt) {

  # Ugly argument check, since it doesn't provide nice error message.
  stopifnot(is.data.frame(.data) | is.null(.data))

  # We want to be able to accept arguments as unquoted names or quoted names.
  # `ensym()`  captures only names, not expressions
  msl_sym <- rlang::ensym(.msl)
  date_sym<- rlang::ensym(.dt)

  msl <- rlang::eval_tidy(msl_sym, .data)
  the_date <- rlang::eval_tidy(date_sym, .data)

  # We use generalized least squares so we can properly account for
  # autocorrelation in estimating the error on the slope.

  # IDea:  add parameter to select units for estimate.s?
  the_gls <- nlme::gls(msl ~ the_date, correlation = corAR1())

  ccs <- as.data.frame(summary(the_gls)$tTable)
  EST <- ccs$Value[2] * 365.25 * 1000
  SE <- ccs$Std.Error[2]   * 365.25 * 1000
  CI <- 1.96*SE
  tibble(Estimate = EST, Std_Err = SE, CI_95 =  CI)
}
