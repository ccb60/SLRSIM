
# TODO:  Consider building this function explicitly off of `floodfreq()`.

#' Extract annual flood counts
#'
#' The function accepts historic data on water level indexed by date and time
#' and produces a data frame showing the number of days each year that
#' experienced at least some flooding. Flooding is defined as an observation of
#' water level above a user-defined flood level.
#'
#' The related function, `floodfreq()` offers calculation of similar statistics
#' over an arbitrary period of time.  The function `floodmean()`, calculates
#' these statistics year by year, and reports observed mean and standard error
#' of the yearly statistics. A graphica lalternative is provided by
#' `floodgraph`.
#'
#' @param .data   A source data frame. can be NULL if .dt and .wl are defined
#'                in the enclosing environment.
#' @param .dt     Dates and times.  A data column in .data or a data vector
#'                defined in the enclosing environment that contains date-time
#'                information.  Must inherit from POSIXct.
#' @param .wl     Water level.  A numerical data column in .data or a numerical
#'                vector defined in the enclosing environment.
#' @param .fldlvl The water level that defines a flood event. Needs to be
#'                expressed in the same units and the same datum as the water
#'                level data.)
#'
#' @return A data frame continuing the following values:
#' \describe{
#'   \item{year}{The calander year for the folowing statistics.}
#'   \item{days}{Total number of days that year with (some) data. No effort is
#'         to check if data that day is complete. }
#'   \item{flood_days}{Days with at  least one observation of a water level
#'         above the user-defined flood level.}
#'   \item{daily_p_flood}{Daily probability of flooding, as estimated
#'        crudely as the proportion of all days with data that show at least one
#'        observation above the user-defined flood level.}
#'   \item{adjusted}{Estimated number of flood events this year if data had been
#'        complete.  Estimated as \eqn{daily_p_flood \times 365.25}.}
#'        }
#' @export
#'
#' @examples
floodcounts <- function(.data, .dt, .wl, .fldlvl) {

  stopifnot(inherits(.data, 'data.frame') || is.null(.data))

  .dt <- rlang::enquo(.dt)
  .wl <- rlang::enquo(.wl)

  dt <- rlang::eval_tidy(.dt, .data)
  wl <- rlang::eval_tidy(.wl, .data)

  stopifnot(length(.fldlvl) == 1 && is.numeric(.fldlvl))

  stopifnot(inherits(dt, 'POSIXct'))

  message(Sys.time())
  response <- dplyr::tibble(theDT = dt, obs_wl = wl) %>%
    dplyr::mutate(theDate = as.Date(theDT),
                  year = as.numeric(format(theDate, format = '%Y')))

  message(Sys.time())
  response <- response %>%
    dplyr::group_by(theDate)  %>%
    dplyr::summarize(year = first(year),
                     exceeded = any(obs_wl > .fldlvl),
                     .groups = 'drop')

  message(Sys.time())
  response <- response %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(days = sum(! is.na(exceeded)),
                     flood_days = sum(exceeded, na.rm = TRUE),
                     daily_p_flood = flood_days/days,
                     adjusted = 365.25 * daily_p_flood,
                     .groups = 'drop')
  message(Sys.time())
  return(response)
 }
